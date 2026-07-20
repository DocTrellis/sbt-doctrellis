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

cp fixtures/broken-link.md docs/broken-link.md
wait_for "a non-terminal rebuild failure" grep -Fq 'rebuild failed; continuing to serve the last successful site' "$preview_log"
grep -Fq 'Live watch revision marker.' target/doctrellis/site/index.html

rm docs/broken-link.md
printf '\nRecovered live preview marker.\n' >>docs/index.md
wait_for "recovery after a failed rebuild" grep -Fq 'Recovered live preview marker.' target/doctrellis/site/index.html

stop_preview
