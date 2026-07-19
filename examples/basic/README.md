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

Open `http://127.0.0.1:4242`, then press Ctrl+C to stop it. The current preview
server builds once at startup; restart it after editing the documentation.

To exercise the extension, open this directory as a trusted VS Code workspace
with the local DocTrellis VSIX installed. Run **DocTrellis: Start Preview**. The
extension reads this example's `.doctrellis.json`, starts the same sbt task,
waits for the URL, and opens it.
