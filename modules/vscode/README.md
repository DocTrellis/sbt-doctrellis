# DocTrellis for VS Code

This desktop extension starts the command declared in a workspace's `.doctrellis.json`, waits for its local HTTP preview, and opens that preview in VS Code. It does not build documentation itself; the `sbt-doctrellis` plugin owns the mdoc and Laika build.

Use **DocTrellis: Start Preview** from the Command Palette in a trusted
workspace. When ready, the extension opens a `DocTrellis Preview` Webview tab
containing the remote-aware preview URL. Repeated opens and restarts reuse the
workspace's panel, while stop, startup failure, and unexpected process exit are
shown as connection states in the same tab. Multi-root folders have independent
panels. With a version 2 IPC preview and the current plugin, successful
documentation rebuilds refresh an existing panel automatically at the same
stored URL. Events do not reveal the panel or recreate one that the user
closed.

Use **DocTrellis: Open in External Browser** when the preview should leave VS
Code. Both the Webview iframe and external command apply VS Code's
`asExternalUri` mapping for Remote SSH and Dev Container extension hosts. Logs
are available through **DocTrellis: Show Output**. External browsers receive
the rebuilt site but require a manual page refresh.

The extension is currently packaged for local installation only and is not published to the VS Code Marketplace.

For sbt 2 workspaces, prefer schema version 2 with arguments
`["--server", "--batch", "set doctrellisPort := 0", "doctrellisPreview"]`
and endpoint protocol `doctrellis-preview-v1`. The plugin binds HTTP to
`127.0.0.1:0` and announces the actual positive URL over an authenticated
per-workspace control channel. This keeps the foreground
preview JVM in the process tree started by the extension. Stop first requests a
graceful shutdown and then, only after a bounded timeout, forces that exact
owned group on POSIX or PID tree on Windows. Workspace removal and extension
deactivation use the same cleanup path. Existing version 1 argument vectors
remain valid and are never rewritten.

Version 1 remains the fixed-URL compatibility mode and continues to poll its
configured URL. It has no authenticated publication-event channel, so a
version 1 Webview must be closed and reopened to load a rebuild. Version 2 has
no duplicate `url` field. Unix hosts use a
current-user-owned, non-symlinked runtime directory and a short session-unique
domain socket; Windows uses a token-authenticated loopback channel. IPC remains
on the extension host in Remote SSH and Dev Containers, while only HTTP goes
through `asExternalUri`. A second window can observe and reuse a live workspace
URL after correlated ready and ping responses, but holds a read-only lease and
does not stop the process owned by the first window. Owner and observer
connections both receive successful-publication refresh events when the
current plugin supports them; older plugins ignore the additive opt-in and
remain compatible.
A concurrent cold start also carries a random launch ID through the child
environment, endpoint metadata, and ready response, so a losing window cannot
adopt or stop the winning window's process.
Restart from an observing window releases and reacquires the read-only lease;
it does not restart the process owned by the first window. The announced IPC
mode requires the plugin host to remain `127.0.0.1`.

On Windows, set `command` to a native executable such as an `sbt.exe`
package-manager shim or its absolute path. The extension deliberately uses
direct process spawning, so `.bat` and `.cmd` launchers are not passed through
a shell. Keep the same foreground argument vector, including the version 2
`set doctrellisPort := 0` command when dynamic announcement is used.
The native launcher must remain the foreground tree root. If it detaches and
disappears before Windows can address its tree, cleanup fails explicitly rather
than guessing at unrelated processes by name or port.
