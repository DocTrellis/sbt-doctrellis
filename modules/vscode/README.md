# DocTrellis for VS Code

This desktop extension starts the command declared in a workspace's `.doctrellis.json`, waits for its local HTTP preview, and opens that preview in VS Code. It does not build documentation itself; the `sbt-doctrellis` plugin owns the mdoc and Laika build.

Use **DocTrellis: Start Preview** from the Command Palette in a trusted workspace. Logs are available through **DocTrellis: Show Output**.

The extension is currently packaged for local installation only and is not published to the VS Code Marketplace.
