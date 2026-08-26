set -eu

plugin_version_file=${1:?plugin version file is required}
if [ ! -f "$plugin_version_file" ]; then
  echo "plugin version file does not exist: $plugin_version_file" >&2
  exit 1
fi
plugin_version=$(sed -n '1p' "$plugin_version_file")
if [ -z "$plugin_version" ]; then
  echo "plugin version file is empty: $plugin_version_file" >&2
  exit 1
fi
classpath_artifact_file=${2:?classpath artifact file is required}
if [ ! -f "$classpath_artifact_file" ]; then
  echo "classpath artifact file does not exist: $classpath_artifact_file" >&2
  exit 1
fi
classpath_artifact=$(sed -n '1p' "$classpath_artifact_file")
if [ ! -e "$classpath_artifact" ]; then
  echo "classpath artifact does not exist: $classpath_artifact" >&2
  exit 1
fi

preview_log=live-preview.log
preview_pid=
preview_group=
preview_log_shown=0

show_preview_log_tail() {
  preview_log_shown=1
  if [ -f "$preview_log" ]; then
    echo "last 240 lines of $preview_log:" >&2
    tail -n 240 "$preview_log" >&2
  else
    echo "preview log was not created: $preview_log" >&2
  fi
}

preview_group_is_alive() {
  [ -n "$preview_group" ] && kill -0 "-$preview_group" 2>/dev/null
}

assert_preview_cleanup() {
  cleanup_failed=0

  if [ -f "$preview_log" ] &&
    grep -Fq 'preview cleanup did not finish' "$preview_log"; then
    echo "preview JVM reported incomplete shutdown cleanup" >&2
    cleanup_failed=1
  fi

  for owned_path in \
    target/doctrellis/.mdoc.doctrellis-????????-????-????-????-????????????.* \
    target/doctrellis/..mdoc.doctrellis-????????-????-????-????-????????????.* \
    target/doctrellis/.site.doctrellis-????????-????-????-????-????????????.* \
    target/doctrellis/..site.doctrellis-????????-????-????-????-????????????.*; do
    if [ -e "$owned_path" ] || [ -L "$owned_path" ]; then
      echo "preview shutdown left an owned temporary path: $owned_path" >&2
      cleanup_failed=1
    fi
  done

  [ "$cleanup_failed" -eq 0 ]
}

stop_preview() {
  stop_failed=0
  forced=0

  if preview_group_is_alive; then
    kill -TERM "-$preview_group" 2>/dev/null || true
    attempt=0
    while preview_group_is_alive && [ "$attempt" -lt 40 ]; do
      attempt=$((attempt + 1))
      sleep 1
    done

    if preview_group_is_alive; then
      forced=1
      kill -KILL "-$preview_group" 2>/dev/null || true
      attempt=0
      while preview_group_is_alive && [ "$attempt" -lt 5 ]; do
        attempt=$((attempt + 1))
        sleep 1
      done
    fi
  fi

  if [ -n "$preview_pid" ]; then
    wait "$preview_pid" 2>/dev/null || true
  fi

  if preview_group_is_alive; then
    echo "preview process group $preview_group survived SIGKILL" >&2
    stop_failed=1
  fi
  if [ "$forced" -ne 0 ]; then
    echo "preview process group $preview_group required SIGKILL" >&2
    stop_failed=1
  fi
  if ! assert_preview_cleanup; then
    stop_failed=1
  fi

  if [ "$stop_failed" -ne 0 ] && [ "$preview_log_shown" -eq 0 ]; then
    show_preview_log_tail
  fi

  preview_pid=
  preview_group=
  [ "$stop_failed" -eq 0 ]
}

finish() {
  status=$?
  trap - EXIT
  if [ "$status" -ne 0 ] && [ "$preview_log_shown" -eq 0 ]; then
    show_preview_log_tail
  fi
  if ! stop_preview; then
    status=1
  fi
  exit "$status"
}

wait_for() {
  description=$1
  shift
  attempt=0
  until "$@"; do
    if ! preview_group_is_alive; then
      echo "preview exited while waiting for $description" >&2
      show_preview_log_tail
      exit 1
    fi
    attempt=$((attempt + 1))
    if [ "$attempt" -ge 120 ]; then
      echo "timed out waiting for $description" >&2
      show_preview_log_tail
      exit 1
    fi
    sleep 1
  done
}

rebuild_failure_count() {
  grep -Fc \
    'rebuild failed; continuing to serve the last successful site' \
    "$preview_log" 2>/dev/null || true
}

has_rebuild_failure_count() {
  minimum=$1
  actual=$(rebuild_failure_count)
  [ "$actual" -ge "$minimum" ]
}

mdoc_reload_count() {
  grep -Fc 'reloading the mdoc engine' "$preview_log" 2>/dev/null || true
}

start_preview() {
  sbt_command=$(command -v sbt)
  if command -v setsid >/dev/null 2>&1; then
    setsid "$sbt_command" -Dplugin.version="$plugin_version" --server --batch \
      'set doctrellisPort := 0' doctrellisPreview >"$preview_log" 2>&1 &
  elif command -v perl >/dev/null 2>&1; then
    perl -MPOSIX -e \
      'POSIX::setsid() >= 0 or die "setsid failed: $!\n"; exec {$ARGV[0]} @ARGV or die "exec failed: $!\n"' \
      "$sbt_command" -Dplugin.version="$plugin_version" --server --batch \
      'set doctrellisPort := 0' doctrellisPreview >"$preview_log" 2>&1 &
  else
    echo "live preview scripted test requires setsid or perl" >&2
    exit 1
  fi

  preview_pid=$!
  preview_group=$preview_pid
  attempt=0
  while ! preview_group_is_alive && [ "$attempt" -lt 5 ]; do
    attempt=$((attempt + 1))
    sleep 1
  done
  if ! preview_group_is_alive; then
    echo "preview launcher did not create its owned process group" >&2
    show_preview_log_tail
    kill -TERM "$preview_pid" 2>/dev/null || true
    wait "$preview_pid" 2>/dev/null || true
    preview_pid=
    preview_group=
    exit 1
  fi
}

trap finish EXIT
trap 'exit 130' INT
trap 'exit 143' TERM

start_preview

wait_for "preview startup" grep -Fq 'DocTrellis preview ready at http://127.0.0.1:' "$preview_log"

printf '\n## Live preview edit\n\nLive watch revision marker.\n' >>docs/index.md
wait_for "the first watched rebuild" grep -Fq 'Live watch revision marker.' target/doctrellis/site/index.html

refresh_classes=target/classpath-refresh
refresh_artifact=target/classpath-refresh.jar
if ! command -v javac >/dev/null 2>&1; then
  echo "javac is required for the classpath refresh test" >&2
  exit 1
fi
if ! command -v jar >/dev/null 2>&1; then
  echo "jar is required for the classpath refresh test" >&2
  exit 1
fi
mkdir -p "$refresh_classes"
javac --release 17 -d "$refresh_classes" fixtures/FreshValue.java
jar cf "$refresh_artifact" -C "$refresh_classes" \
  org/doctrellis/example/FreshValue.class
mv -f "$refresh_artifact" "$classpath_artifact"
if [ -L "$classpath_artifact" ]; then
  echo "classpath refresh followed the sbt output symlink" >&2
  exit 1
fi
printf '\nClasspath refresh marker.\n' >>docs/index.md
wait_for "the classpath-triggered mdoc reload" grep -Fq \
  'DocTrellis: classpath changed; reloading the mdoc engine' "$preview_log"
wait_for "the classpath-triggered rebuild" grep -Fq \
  'Classpath refresh marker.' target/doctrellis/site/index.html
grep -Fq 'classpath value after' target/doctrellis/site/index.html

reload_count_before=$(mdoc_reload_count)
failure_count_before=$(rebuild_failure_count)
cp fixtures/broken-mdoc.md docs/broken-mdoc.md
wait_for "a non-terminal mdoc failure" has_rebuild_failure_count \
  "$((failure_count_before + 1))"
grep -Fq 'Classpath refresh marker.' target/doctrellis/site/index.html

rm docs/broken-mdoc.md
printf '\nRecovered mdoc preview marker.\n' >>docs/index.md
wait_for "recovery after an mdoc failure" grep -Fq \
  'Recovered mdoc preview marker.' target/doctrellis/site/index.html
reload_count_after=$(mdoc_reload_count)
if [ "$reload_count_after" -ne "$reload_count_before" ]; then
  echo "a document error unnecessarily reloaded the mdoc engine" >&2
  exit 1
fi

failure_count_before=$(rebuild_failure_count)
cp fixtures/broken-link.md docs/broken-link.md
wait_for "a non-terminal Laika failure" has_rebuild_failure_count \
  "$((failure_count_before + 1))"
grep -Fq 'Recovered mdoc preview marker.' target/doctrellis/site/index.html

rm docs/broken-link.md
printf '\nRecovered live preview marker.\n' >>docs/index.md
wait_for "recovery after a failed rebuild" grep -Fq 'Recovered live preview marker.' target/doctrellis/site/index.html

stop_preview
