# DocTrellis for VS Code

This desktop extension starts the command declared in a workspace's `.doctrellis.json`, waits for its local HTTP preview, and opens that preview in VS Code. It does not build documentation itself; the `sbt-doctrellis` plugin owns the mdoc and Laika build.

Use **DocTrellis: Start Preview** from the Command Palette in a trusted
workspace. When ready, the extension opens a `DocTrellis Preview` Webview tab
containing the remote-aware preview URL. Repeated opens and restarts reuse the
workspace's panel, while stop, startup failure, and unexpected process exit are
shown as connection states in the same tab. Multi-root folders have independent
panels.

Use **DocTrellis: Open in External Browser** when the preview should leave VS
Code. Both the Webview iframe and external command apply VS Code's
`asExternalUri` mapping for Remote SSH and Dev Container extension hosts. Logs
are available through **DocTrellis: Show Output**.

The extension is currently packaged for local installation only and is not published to the VS Code Marketplace.

For sbt 2 workspaces, configure the version 1 preview arguments as
`["--server", "--batch", "doctrellisPreview"]`. This keeps the foreground
preview JVM in the process tree started by the extension. Stop first requests a
graceful shutdown and then, only after a bounded timeout, forces that exact
owned group on POSIX or PID tree on Windows. Workspace removal and extension
deactivation use the same cleanup path. Existing version 1 argument vectors
remain valid and are never rewritten.

On Windows, set `command` to a native executable such as an `sbt.exe`
package-manager shim or its absolute path. The extension deliberately uses
direct process spawning, so `.bat` and `.cmd` launchers are not passed through
a shell. Keep the same `--server --batch doctrellisPreview` argument vector.
The native launcher must remain the foreground tree root. If it detaches and
disappears before Windows can address its tree, cleanup fails explicitly rather
than guessing at unrelated processes by name or port.
