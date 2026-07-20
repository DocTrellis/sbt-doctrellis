# ADR 0003: Scala.js VS Code extension

- Status: Accepted
- Date: 2026-07-19
- Amended by: ADR 0007

## Context

The extension runs in VS Code's Node.js extension host. The repository uses sbt
2, where adding a large facade-generation/bundling stack would increase build
power and compatibility risk for a small API surface.

Scala.js 1.22.0 is the first Scala.js release with sbt 2 support. Compatibility
and packaging choices were checked against the Scala.js and VS Code
documentation and artifact repositories on 2026-07-19.

## Decision

Use Scala.js 1.22.0 with Scala 3.3.8 LTS for all extension subprojects. Use Cats
Effect 3.7.0 for resource-safe asynchronous lifecycle logic and
MUnit/Cats Effect support for tests. Compile production code with a focused
WartRemover 3.6.1 error set and format Scala 3 code with significant indentation
where it remains clear.

Generate Node-oriented CommonJS with source maps and `FewestModules`. Export
`activate` and `deactivate` with `@JSExportTopLevel`, including the module ID
`extension`, so the output is the package entry point `dist/extension.js`.

Do not bundle `vscode`; import it as the runtime-provided module. Do not use the
ScalablyTyped sbt plugin. Maintain hand-written facades containing only the VS
Code, `child_process`, filesystem, path, and HTTP members the implementation
actually calls.

Package with the project-local `@vscode/vsce` 3.9.2. The npm package is private,
marked `UNLICENSED`, and provides only a local VSIX command—no Marketplace
publish command.

Workspace Trust is enforced in both `package.json` command enablement and at
runtime. Default preview display uses a `window.createWebviewPanel` registry
keyed by the canonical workspace URI. Before rendering an iframe, resolve its
HTTP URL with `env.asExternalUri` so the same behavior works from a remote
extension host. Reuse and reveal an existing panel for the same workspace;
keep other workspace panels independent and unregister panels on disposal.

The wrapper Webview disables scripts and command URIs, declares no local
resource roots, applies a default-deny CSP with only the resolved iframe origin,
and gives the iframe an empty sandbox. Lifecycle states replace the iframe with
escaped status content after a stop, startup failure, or unexpected exit. A
separate **Open in External Browser** command also resolves the URI and then
uses `env.openExternal`.

## Consequences

- The generated JavaScript is directly loadable by the Node extension host.
- The minimal facade surface is reviewable and avoids generated bindings and a
  bundler.
- API additions require a small manual facade update.
- Domain/application tests do not require the real `vscode` module.
- Source maps and repository launch configuration support an Extension
  Development Host debugging loop.

## Sources checked

- [Scala.js 1.22.0 announcement](https://www.scala-js.org/news/2026/06/20/announcing-scalajs-1.22.0/)
- [Scala.js exports to JavaScript](https://www.scala-js.org/doc/interoperability/export-to-javascript.html)
- [VS Code extension anatomy](https://code.visualstudio.com/api/get-started/extension-anatomy)
- [VS Code built-in commands](https://code.visualstudio.com/api/references/commands)
- [VS Code Workspace Trust](https://code.visualstudio.com/api/extension-guides/workspace-trust)
- [VS Code extension packaging](https://code.visualstudio.com/api/working-with-extensions/publishing-extension)
- [Cats Effect getting started](https://typelevel.org/cats-effect/docs/getting-started)
