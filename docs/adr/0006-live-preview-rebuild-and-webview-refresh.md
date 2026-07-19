# ADR 0006: Live preview rebuild and WebView refresh

- Status: Accepted
- Date: 2026-07-20
- Amends: ADR 0002
- Extends: ADR 0005

## Context

`doctrellisPreview` currently runs `doctrellisBuild` once and then serves the
generated site until the process is stopped. Editing Markdown, Laika
configuration, or a static asset under `doctrellisSourceDirectory` therefore
requires a full preview restart. The VS Code extension can reuse a preview
panel across that restart, but it does not observe documentation changes.

The build belongs to the sbt plugin, while the extension owns process and
WebView UX. Moving file watching into the extension would duplicate knowledge
of plugin inputs, make direct CLI preview behave differently, and introduce a
race between a filesystem notification and completion of the mdoc/Laika
pipeline.

The WebView wrapper also deliberately disables scripts and embeds the preview
in an empty-sandbox iframe. Live reload must not weaken that policy or inject a
script into generated documentation. VS Code's extension-host implementation
also skips a WebView HTML assignment when the new string equals the current
string, so merely rendering the same `Ready(url)` state again cannot be the
refresh mechanism.

## Decision

Make `doctrellisPreview` a watch-mode task by default. Keep `doctrellisBuild`
as the explicit one-shot build. This changes the first-release limitation
recorded in ADR 0002 without changing ownership of the build pipeline.

### Change detection and build scheduling

The preview JVM watches the canonical `doctrellisSourceDirectory` with the JDK
filesystem watch service. Register every real directory below the source root,
do not follow directory symlinks, register newly created directories, and also
watch the source root's parent so deletion and recreation of the root can be
recovered. Filter events from that parent to the source-root entry; changes to
siblings do not invalidate the site. Create, modify, and delete events inside
the source tree all invalidate the complete site; an overflow event is also a
full invalidation. The layout's existing overlap checks keep generated mdoc and
site directories outside the watched source tree and prevent a successful
build from triggering itself.

Coalesce editor save sequences behind a short internal quiet period. Run at
most one build at a time. If another invalidation arrives during a build, mark
the build dirty and run exactly one more build after the current attempt. Do
not expose debounce timing as public configuration until a demonstrated use
case requires it.

Watching covers every file below `doctrellisSourceDirectory`, including
Markdown, `directory.conf`, and copied assets. Changes to Scala sources,
project settings, the resolved classpath, or files reached through external
symlinks remain restart boundaries. The implementation performs a complete
mdoc and Laika build for each invalidation; incremental document compilation is
separate future work.

Resolve the sbt settings and classpath needed by the build before entering the
blocking preview loop, then invoke a reusable build program from both the
initial task execution and the watcher. Do not attempt to re-evaluate sbt task
macros from a watcher thread.

### Publication and failure behavior

The initial build must succeed and produce `index.html` before the HTTP server
is announced, preserving the existing startup contract. Later builds render
into session-unique staging directories rather than deleting the currently
served site up front.

After a staged build succeeds and contains `index.html`, publish it while
holding an exclusive publication lock shared with the static HTTP handler.
HTTP requests take the corresponding read lock, so they observe either the
previous complete site or the next complete site, never a partially replaced
tree. A request holds its read lock until its response body has finished
streaming. Use same-filesystem sibling staging and backup directories so a
failed replacement can restore the last successful site. Clean session-owned
staging and backup directories during normal shutdown and stale-session startup
cleanup without deleting user-owned paths.

A rebuild failure is non-terminal: log the diagnostic through sbt, discard the
failed staging output, keep serving the last successful site, and retry after
the next invalidation. Failure of the watcher itself is terminal because
silently serving a permanently stale preview would violate the live-preview
contract.

### Refresh notification

The plugin emits a notification only after successful publication. Extend the
authenticated `doctrellis-preview-v1` channel from ADR 0005 compatibly:

- a client opts in by adding the numeric field `"siteEvents": 1` to its
  `hello` request;
- the server sends that client an unsolicited `siteChanged` message containing
  the session ID and a monotonically increasing, session-local revision; and
- the server never sends unsolicited events to a client that did not opt in.

Route request responses and unsolicited events through one connection-local
serialized frame writer. A publication thread must never write directly to the
same output stream as the connection's request loop, and it must release the
publication lock before enqueueing or sending `siteChanged`. This preserves
newline-delimited frame boundaries. Releasing the lock before handing off the
event also prevents a blocked IPC write from extending the exclusive
publication critical section and stalling every HTTP request waiting for its
read lock.

For example:

```json
{"version":1,"type":"siteChanged","sessionId":"...","revision":2}
```

The opt-in is additive because existing protocol parsers accept and ignore
unknown atomic fields. A new extension can therefore connect to an older
plugin, which ignores `siteEvents`, and a new plugin will not send an unknown
message to an older extension. A revision is a coalescing and ordering value,
not a durable content version; it resets with each preview session.

Every owner or observer connection may opt in. On `siteChanged`, the extension
updates an existing panel for that workspace with the already stored ready URL
without revealing or creating a panel. The extension carries the accepted
revision into the panel renderer and includes it in inert wrapper markup, such
as a `data-doctrellis-revision` attribute on the iframe. The rendered HTML must
therefore differ for every accepted revision; do not rely on assigning an
identical string to `webview.html`. VS Code then applies the new HTML and
recreates the iframe, while the server's existing `Cache-Control: no-store`
response causes it to fetch the published files. Reject duplicate or decreasing
revisions for the same session. Closing the panel does not stop watching;
opening it later loads the current site.

Do not add WebView scripts, `postMessage` handlers, iframe script permissions,
server-sent events, or markup injected into the generated site. The
script-free wrapper, default-deny CSP, empty iframe sandbox, and remote URI
resolution from ADR 0003 remain unchanged.

Fixed-URL version 1 configurations and direct CLI browsers have no authenticated
event channel. They receive automatic rebuilds when the command is the new
`doctrellisPreview`. A normal browser requires a manual page refresh; a version
1 WebView requires closing and reopening the preview panel or restarting the
preview because the sandboxed panel has no refresh command. The VS Code
automatic refresh applies to version 2 IPC previews. Arbitrary preview commands
remain supported and are not assumed to implement DocTrellis events.

## Consequences

- Documentation edits rebuild without restarting the preview JVM or changing
  its dynamic HTTP URL.
- The plugin remains the single owner of source discovery and build ordering;
  the extension reacts only to a successful publication signal.
- Failed edits leave a usable last-known-good preview and remain visible in the
  DocTrellis output channel.
- Multiple rapid filesystem events are bounded to serialized full builds, but
  large sites may still rebuild slowly until incremental compilation is added.
- On macOS, OpenJDK 25 implements `WatchService` with a two-second polling
  interval rather than native FSEvents. Change detection may therefore take
  several seconds and polling work grows with the number of registered
  directories; the debounce quiet period is additional to that platform delay.
- A slow or stalled HTTP response holds the publication read lock and delays
  the next site's publication. This is accepted for the loopback-only
  development server, but means publication latency is not strictly bounded by
  build time.
- Recreating the sandboxed iframe returns it to the preview root. Preserving an
  in-frame navigation path would require a trusted navigation protocol or a
  weaker sandbox and is deferred.
- Watch-service behavior, publication rollback, change-during-build handling,
  event opt-in compatibility, observer notifications, and resource cleanup
  require unit and scripted integration coverage.

## Alternatives considered

- A VS Code `FileSystemWatcher` was rejected because it would omit direct CLI
  preview and cannot know when the plugin has safely published a build.
- Injecting a live-reload script or enabling WebView scripts was rejected
  because it weakens the accepted rendering boundary and modifies generated
  content for a development concern.
- Periodically polling or refreshing the iframe was rejected because it adds
  permanent traffic, can reload during a build, and provides poorer failure
  semantics than a publication event.
- Serving immutable generation directories behind an atomic pointer swap would
  prevent slow HTTP clients from delaying publication. It was deferred because
  generations need leases and cleanup while
  `doctrellisOutputDirectory` must continue to name the current materialized
  site on POSIX and Windows. Revisit this if real preview workloads make the
  read/write-lock tradeoff observable.
- sbt continuous execution alone was rejected because the blocking HTTP server
  and extension-owned process lifecycle still need one coordinated foreground
  process.

## Sources checked

- [VS Code 1.74 `ExtHostWebview.html` setter](https://github.com/microsoft/vscode/blob/1.74.0/src/vs/workbench/api/common/extHostWebview.ts#L100-L103)
- [OpenJDK 25 macOS/BSD watch-service selection](https://github.com/openjdk/jdk/blob/jdk-25-ga/src/java.base/macosx/classes/sun/nio/fs/BsdFileSystem.java#L51-L56)
- [OpenJDK 25 polling watch-service interval](https://github.com/openjdk/jdk/blob/jdk-25-ga/src/java.base/share/classes/sun/nio/fs/PollingWatchService.java#L62-L63)
