# DocTrellis basic example

This is a minimal consumer of `sbt-doctrellis`. Its Markdown contains an mdoc
block and GFM task lists, `directory.conf` makes `index.md` Laika's title
document and defines the navigation order, and the plugin selects Laika's
Helium theme. `checklist.md` is a second, plan-style task list fixture.

The development plugin is not published remotely. From the repository root,
publish the current snapshot locally and then build the example:

```console
sbt --batch 'sbtDoctrellis / publishLocal'
cd examples/basic
sbt --batch doctrellisBuild
```

The processed Markdown is written to `target/doctrellis/mdoc/index.md` and the
site entry point to `target/doctrellis/site/index.html`. The plan fixture is
rendered to `target/doctrellis/site/checklist.html`.

Start the blocking local server with:

```console
sbt doctrellisPreview
```

Open `http://127.0.0.1:4242`, then press Ctrl+C to stop it. Preview watches the
complete `docs/` tree and publishes successful rebuilds without restarting the
JVM. A failed edit leaves the last successful site available and the next edit
retries the build. Refresh the browser manually to load a published rebuild.
The `4242` default is retained for this direct CLI workflow.

To exercise the extension, open this directory as a trusted VS Code workspace
with the local DocTrellis VSIX installed. Run **DocTrellis: Start Preview**. The
extension reads this example's `.doctrellis.json` and starts sbt with
`--server --batch "set doctrellisPort := 0" doctrellisPreview`, keeping the
foreground JVM in its owned process tree. The plugin binds an OS-assigned port
and announces its actual URL over the workspace IPC channel; no configured URL
duplicates the port. The extension probes that URL and opens it in a reusable
`DocTrellis Preview` Webview tab. Successful publications refresh that existing
version 2 panel automatically, including when this window is observing a
preview owned by another window. A closed panel is not recreated or revealed.
Run **DocTrellis: Stop Preview** to wait for the server and listener to exit, or
**DocTrellis: Open in External Browser** to open the resolved preview URL in the
system browser instead.
