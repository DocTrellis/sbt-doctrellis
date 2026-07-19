# DocTrellis

DocTrellis is a local documentation toolchain for Scala projects. It keeps the
documentation build in sbt and gives VS Code a small, explicit process manager
for starting and displaying that build.

The repository contains two deliverables:

- `sbt-doctrellis` runs mdoc, renders the resulting Markdown with Laika and its
  Helium theme, and serves the generated site on localhost.
- `doctrellis-vscode` reads a workspace-owned `.doctrellis.json`, starts the
  configured preview executable, waits for its URL, and opens it from VS Code.

The extension deliberately does not generate documentation. The complete data
flow is:

```text
VS Code command
  -> DocTrellis VS Code extension
  -> sbt doctrellisPreview
  -> mdoc
  -> Laika + Helium
  -> localhost preview server
  -> VS Code preview
```

## Repository layout

```text
modules/sbt-doctrellis/       sbt 2 AutoPlugin and scripted tests
modules/vscode/domain/       parsed domain values and lifecycle model
modules/vscode/application/  Preview algebra and effect-polymorphic program
modules/vscode/extension/    Interpreters, composition root, and Scala.js exports
modules/vscode/schema/       packaged .doctrellis.json schema
examples/basic/              minimal plugin and extension example
docs/adr/                    architecture decisions and compatibility record
```

The single repository keeps the plugin/config/extension contract testable as
one change while preserving independent module and package boundaries. The VS
Code implementation uses a tagless-final style with inward dependencies: the
domain owns platform-independent values and rules, the application defines an
algebra and composes it into a program, and the extension supplies Node/VS Code
interpreters and the composition root.

## Requirements

- JDK 25 (the LTS used in CI; sbt 2 requires JDK 17 or newer)
- sbt 2.0.3
- Node.js 22 or newer for local VSIX tooling
- VS Code 1.74 or newer for running/debugging the extension

All npm tools are project-local. In particular, packaging uses the pinned
`@vscode/vsce` dependency and does not require a global `vsce` installation.

## Use the sbt plugin

Until a public artifact exists, first publish the development plugin to the
local sbt repository:

```console
sbt --batch 'sbtDoctrellis / publishLocal'
```

This source repository also consumes that local artifact to preview its own
ADRs. After publishing it, open the repository root in VS Code and run
**DocTrellis: Start Preview**. The root `.doctrellis.json` starts the root
project, whose `docs/index.md` links the architecture decision records. The
Preview command opts into this local artifact explicitly, so ordinary builds
and CI do not require it. When developing the plugin itself, run
`sbtDoctrellis / publishLocal` again and restart Preview before checking
behavior changed in the plugin.

Add the plugin to `project/plugins.sbt`:

```scala
addSbtPlugin("org.doctrellis" % "sbt-doctrellis" % "0.1.0-SNAPSHOT")
```

Enable it in `build.sbt`:

```scala
enablePlugins(org.doctrellis.sbtplugin.DocTrellisPlugin)
```

By default DocTrellis reads `docs/`, writes mdoc output to
`target/doctrellis/mdoc/`, and writes HTML to
`target/doctrellis/site/`. Build once with:

```console
sbt doctrellisBuild
```

When the entry document is named `index.md`, add this to
`docs/directory.conf` so Laika treats it as the root title document:

```hocon
laika.titleDocuments.inputName = index
laika.navigationOrder = [index.md]
```

Or build and serve the site at `http://127.0.0.1:4242` with:

```console
sbt doctrellisPreview
```

`doctrellisPort` still defaults to `4242` for direct CLI use. Setting it to
`0` asks the operating system to choose and bind a free port atomically; the
server reports the positive port returned by its bound address.

### GFM task lists

DocTrellis supports GitHub Flavored Markdown task markers in unordered,
ordered, and nested lists. Both lowercase and uppercase checked markers are
accepted, and inline Markdown after the marker is parsed normally:

```markdown
- [ ] unchecked
- [x] **checked**
- [X] [`doctrellisBuild`](https://doctrellis.org) executed

1. [ ] ordered unchecked
2. [x] ordered checked

- [ ] parent
  - [x] nested child
```

A marker is recognized only as the first content in a list item. Marker-like
text in a paragraph, inline or fenced code, and an escaped marker such as
`- \[ ]` is not converted. Rendered checkboxes are disabled because they show
static document state, and task list elements expose `task-list-item` and
`contains-task-list` classes for theme customization. For example:

```html
<input type="checkbox" disabled="disabled" aria-label="Incomplete task">
<input type="checkbox" disabled="disabled" checked="checked" aria-label="Completed task">
```

Task list handling does not relax Laika validation; unrelated unresolved link
references still fail `doctrellisBuild`.

The plugin exposes these settings and tasks:

| Key | Default |
| --- | --- |
| `doctrellisSourceDirectory` | `<project>/docs` |
| `doctrellisMdocDirectory` | `<project>/target/doctrellis/mdoc` |
| `doctrellisOutputDirectory` | `<project>/target/doctrellis/site` |
| `doctrellisHost` | `127.0.0.1` |
| `doctrellisPort` | `4242` |
| `doctrellisBuild` | mdoc, then Laika/Helium |
| `doctrellisPreview` | build, then serve until interrupted |

See [the basic example](examples/basic/README.md) for a complete consumer.

## Configure the VS Code extension

Create `.doctrellis.json` at each workspace folder root:

```json
{
  "version": 2,
  "preview": {
    "command": "sbt",
    "args": [
      "--server",
      "--batch",
      "set doctrellisPort := 0",
      "doctrellisPreview"
    ],
    "workingDirectory": ".",
    "endpoint": {
      "source": "ipc",
      "protocol": "doctrellis-preview-v1"
    },
    "startupTimeoutMs": 90000
  }
}
```

`command` is spawned directly with `args`; it is never assembled into a shell
string. `workingDirectory` is relative to the selected workspace folder and is
resolved before use, including a check that symlinks do not escape the
workspace. Version 2 receives the actual positive HTTP URL from a project-keyed
IPC control channel, then probes and displays that URL. It never finds a free
port and closes it before server startup. Malformed, missing, unexpected, and
version-incompatible fields are rejected before process launch.

For sbt 2, keep `--server` and `--batch` in the configured argument vector as
shown above. Version 2 additionally sets `doctrellisPort := 0`; arbitrary
commands are still passed verbatim. This foreground form keeps the preview JVM in the exact process
tree owned by the extension, so Stop, workspace close, and deactivation can
terminate it and wait for the HTTP listener to close. Existing version 1
configurations remain valid and commands are always used verbatim; the
extension never inserts sbt-specific arguments into arbitrary executables.
Version 1 fixed-URL configurations remain supported without migration and
continue polling their configured URL.
On Windows, `command` must resolve to a native executable such as an
`sbt.exe` package-manager shim (or its absolute path). Node intentionally does
not invoke a shell, so `.bat` and `.cmd` launchers are not directly executable;
the `args` vector remains the same foreground form.
Windows cleanup relies on the spawned native launcher's PID tree remaining
addressable through `taskkill /T`; if a launcher detaches and disappears first,
Stop reports failure rather than targeting processes by name or port.

In a trusted workspace, use these Command Palette commands:

- **DocTrellis: Start Preview**
- **DocTrellis: Stop Preview**
- **DocTrellis: Restart Preview**
- **DocTrellis: Open Preview**
- **DocTrellis: Open in External Browser**
- **DocTrellis: Show Output**

For a multi-root workspace, DocTrellis asks which folder to use. It manages at
most one owned process tree per folder, sends output to the `DocTrellis` Output
Channel, and stops the selected tree when its folder closes or the extension
deactivates. On POSIX it uses a dedicated process group; on Windows it targets
the spawned PID's process tree. Graceful shutdown is attempted first, with a
bounded exact-tree force fallback. It does not start commands automatically.
The default open action
uses one `DocTrellis Preview` Webview panel per workspace folder, reuses that
panel across repeated opens and restarts, and shows stopped, startup-failure,
or unexpected-exit status without closing the tab. Preview HTTP URLs pass
through VS Code's remote URI resolver before they enter either the Webview
iframe or the explicit external-browser command.

For version 2, the Preview JVM owns an authenticated local control channel.
macOS and Linux use a Unix-domain socket; Windows uses a token-authenticated
loopback control socket as the JDK-portable named-pipe equivalent. Endpoint
metadata and sockets live in a current-user-only temporary directory under a
short hash of the canonical workspace path, never in the project. POSIX
runtimes verify that this directory is a real directory owned by the current
UID. A per-workspace file lock serializes ownership and stale cleanup, while
session-unique socket names keep one cleanup attempt from unlinking another
session. A random launch ID correlates a spawned process with its metadata and
ready response, preventing a losing concurrent start from adopting or stopping
the winner. IPC stays on the Remote SSH or Dev Container extension host; only
the announced HTTP URL is passed to `asExternalUri`. A second VS Code window may
reuse the existing URL under a read-only lease after a correlated ready and
ping response. Only the window that spawned the process stops it on folder
close or deactivation; the observing window merely releases its connection.
Restart in an observing window releases and reacquires that read-only lease;
it does not restart the owner window's process. Announced IPC mode also
requires `doctrellisHost := "127.0.0.1"`, while direct fixed-port CLI use keeps
the host configurable.

## Develop and verify

Run the Scala unit tests, plugin integration test, and Scala.js linkers from the
repository root:

```console
sbt --batch testFull
sbt --batch 'sbtDoctrellis / scripted'
sbt --batch 'vscode / Compile / fastLinkJS'
sbt --batch 'vscode / Compile / fullLinkJS'
```

Verify the real example after publishing the snapshot locally:

```console
sbt --batch 'sbtDoctrellis / publishLocal'
cd examples/basic
sbt --batch doctrellisBuild
test -f target/doctrellis/site/index.html
```

For formatting and static checks, the normal compile uses WartRemover, Scalafix
organizes imports, and Scalafmt handles mechanical formatting:

```console
sbt --batch 'scalafixAll; scalafmtAll; scalafmtSbt'
sbt --batch 'scalafmtCheckAll; scalafmtSbtCheck'
```

Scalafix merges imports before Scalafmt adds trailing commas, so the two tools
form one formatting pass. CI runs that pass on a clean checkout and rejects any
resulting change with `git diff --exit-code`.

### Build and install a VSIX

```console
cd modules/vscode
npm ci
npm run vsix
code --install-extension doctrellis-0.1.0.vsix
```

The local package is written to
`modules/vscode/doctrellis-0.1.0.vsix`. Packaging performs a full Scala.js link
and includes the CommonJS `dist/extension.js` entry point, source map, package
metadata, and local JSON schema.

### Run an Extension Development Host

1. Open this repository in VS Code.
2. Select **Run and Debug: DocTrellis: Extension Development Host**.
3. Press F5. The pre-launch task performs a fast Scala.js link.
4. In the new VS Code window, open `examples/basic` and run
   **DocTrellis: Start Preview**.

The build tasks are also available through **Tasks: Run Task**.

## Current limitations

- `doctrellisPreview` builds once when it starts. It does not yet watch Markdown
  files; stop and restart it after documentation changes.
- Preview binds to localhost and is intended for development only. There is no
  GitHub Pages, website deployment, or other remote publishing feature.
- The Webview uses a script-free wrapper, restrictive content security policy,
  and sandboxed iframe. A rendered documentation site that requires iframe
  script permissions is not supported by the initial preview panel.
- GUI Extension Development Host behavior is a manual smoke test; CI verifies
  compilation, linking, package contents, and the headless preview contract.
- Neither the sbt plugin nor the extension is published to Maven Central or the
  VS Code Marketplace. The VSIX is for local installation only.

The repository source is licensed under the BSD 2-Clause License. The currently
unpublished npm package remains marked private and `UNLICENSED` in its package
metadata.
