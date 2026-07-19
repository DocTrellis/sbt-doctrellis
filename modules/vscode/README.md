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
