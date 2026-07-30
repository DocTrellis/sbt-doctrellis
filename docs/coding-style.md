# DocTrellis Project Coding Guide

DocTrellis follows the reusable
[Scala 3 Functional Coding Style Guide](scala-coding-style.md). This document
adds the architecture mapping, platform boundaries, tool configuration, test
scope, and verification rules specific to this repository.

The base guide applies unless this document gives a more specific DocTrellis
rule. A more specific rule refines the base guide; an apparent contradiction
must be resolved explicitly rather than worked around.

This guide applies to the sbt plugin, the Scala.js VS Code extension, tests,
build definitions, and supporting scripts.

## Sources of truth

- `.scalafmt.conf` is the source of truth for mechanical Scala formatting.
- `build.sbt` is the source of truth for the sbt-tpolecat compiler-warning
  policy and enabled WartRemover checks.
- `.scalafix.conf` is the source of truth for import organization.
- The accepted ADRs under `docs/adr/` are the source of truth for architectural
  decisions.
- This guide, the base guide, and the specialized guides linked from this guide
  cover design and review choices that automated tools cannot enforce.

When these sources appear to disagree, do not work around an automated check.
Clarify the rule or update the relevant decision explicitly.

ADR headers record `Status` and the original `Date`. Add `Updated` for a later
substantive factual or editorial revision that does not change the decision.
Use `Amends`/`Amended by` when a later decision changes an earlier one,
`Extends`/`Extended by` when it adds compatible scope, and `Related` for a
non-changing contextual link. Record every relation in both ADRs: pair
`Amends` with `Amended by`, pair `Extends` with `Extended by`, and repeat
`Related` on both sides. Put explanations in the ADR body rather than in the
relation field.

A body reference requires a header relation when the current decision depends
on another ADR's operative behavior or constrains its scope. A citation that
only supplies background without making the other decision part of the current
contract does not.

## Functional architecture in DocTrellis

The VS Code extension realizes the domain, algebra, program, interpreter, and
composition-root roles from the base guide with these module boundaries:

| Module | Architectural role | Owns | Must not depend on |
| --- | --- | --- | --- |
| `modules/vscode/domain` | Domain | Parsed values, state, and pure rules | Cats Effect, Node, VS Code, sbt |
| `modules/vscode/application` | Algebra and program | Capability definitions, use cases, and lifecycle orchestration | Node and VS Code facades |
| `modules/vscode/extension` | Interpreter and composition root | Node/VS Code implementations, resource wiring, and exported entry points | Private inner-module implementation details |
| `modules/sbt-doctrellis` | Separate JVM vertical | Documentation build and local HTTP preview | VS Code extension modules |

Roles do not require one package, trait, or type class each. In particular,
the application module intentionally owns both algebra definitions and the
programs that consume them.

Apply these project dependency rules:

- The extension composition root selects `IO`, constructs the Node and VS Code
  interpreters, and supplies them to application programs.
- The sbt plugin remains a separate JVM vertical because sbt, mdoc, Laika, and
  the preview HTTP server are not VS Code domain concerns.
- Cross-cutting changes to these dependency directions require an ADR.

## DocTrellis boundary values

- `ConfigParser` must parse `.doctrellis.json` into a `PreviewTemplate`,
  rejecting unsupported versions and fields disallowed by the selected schema
  version before launching a preview process.
- `NodeConfig.load` must resolve and canonicalize the configured path within its
  workspace before constructing the `WorkingDirectory` used by
  `PreviewCommand`.
- `PreviewService` and `PreviewAlgebra` must receive translated domain values,
  not `js.Any`, decoded JSON objects, or unresolved configuration paths.
- Domain parsing failures should be structured values such as `ConfigProblem`.

`NodeConfig.load` implements a two-stage boundary: JSON parsing produces a
`PreviewTemplate`, then filesystem resolution produces a `PreviewCommand`.

The base naming conventions are illustrated here by domain nouns such as
`PreviewCommand` and `PreviewStatus`, and by failures named after their
condition, such as `PreviewTimedOut`.

## Scala toolchain policy

### Compiler warnings

The compiler-warning policy is enforced by sbt-tpolecat. The default build
mode treats warnings as errors so production and test code remain warning
free.

- Configure compiler options through sbt-tpolecat keys such as
  `tpolecatScalacOptions` and `tpolecatExcludeOptions`; do not modify
  `scalacOptions` directly.
- For temporary local iteration without fatal warnings, run the
  `tpolecatDevMode` command or launch sbt with `SBT_TPOLECAT_DEV=1`; do not edit
  `build.sbt` to switch modes.

Production WartRemover rules also apply as review rules in test code, even
when the test configuration does not enforce every wart mechanically.

### Import organization

The final project-package import group described by the base guide is
`org.doctrellis`. The `OrganizeImports` profile in `.scalafix.conf` enforces
the complete import policy. Removing unused imports remains a separate compiler
or lint concern.

## Cats Effect runtime boundaries

The extension composition root and the sbt task runtime select `IO` as the
concrete effect. Concrete `IO`, `IORuntime`, `unsafeRunSync`, `unsafeToPromise`,
and `Dispatcher` unsafe methods are allowed only at one of these outer runtime
boundaries:

- an sbt task that must return synchronously;
- an exported VS Code activation or deactivation function; or
- a Node or VS Code callback being transferred into the selected `IO` runtime.

Keep these operations out of the domain and application modules.

## Structured HTTP APIs

For a structured request/response HTTP API owned on both its Scala server and
client sides, follow the
[Tapir HTTP Contract Guide](tapir-http-contracts.md) and
[ADR 0009](adr/0009-tapir-armeria-http-api.md). Shared Tapir endpoints are the
contract source of truth, the server uses the Armeria Cats Effect interpreter,
and JVM clients use the sttp v4 client interpreter with an Armeria backend. The
Tapir guide is the specialized source of truth for that stack's capability,
lifecycle, Unix-domain-socket operating, and verification rules.

DocTrellis applies that stack only to jointly owned structured APIs. It does
not apply to:

- static documentation serving by the sbt plugin;
- a single-purpose local health, callback, or bridge endpoint; or
- the non-HTTP `doctrellis-preview-v1` control protocol.

The preview server remains the JDK `HttpServer`. The control channel remains
newline-delimited JSON over a Unix domain socket or Windows loopback TCP under
ADR 0005. Do not add Armeria to the sbt plugin merely to replace either
boundary.

ADR 0009 records the stack selection. Replacing an existing transport,
selecting another server or JVM client interpreter, or adding a WebSocket
exception requires an ADR that supersedes or narrowly amends that decision.
The Tapir guide's Unix-domain-socket checklist applies only to a future
JVM-to-JVM local HTTP API approved by such a decision, not to the current
control protocol.

## Scala.js and VS Code boundaries

- Keep facades in `modules/vscode/extension/.../facade`.
- Declare only the JavaScript members that production code calls.
- Keep `js.Dynamic`, `js.Any`, casts, `null`, and `undefined` inside the
  infrastructure boundary (`org.doctrellis.vscode.infrastructure`).
- A facade declaration contains no business logic.
- Keep the `vscode` module external; the VS Code extension host provides it.
- Export only the required CommonJS entry points.
- Instantiate effect-polymorphic application code with `IO` at extension
  assembly, then transfer host callbacks through the owned `Dispatcher[IO]`.
- Restrict mutable lifecycle handles to the extension entry point or a tightly
  scoped interpreter when the host API requires mutation.
- Resolve remote preview URLs through the supported VS Code URI APIs before
  displaying them.
- Treat workspace configuration as executable authority. Process-starting
  commands must require Workspace Trust at both manifest and runtime
  boundaries.

Spawn child processes directly with `shell = false`. Keep the executable and
argument vector separate, log the resolved working directory, and define a
bounded TERM/KILL shutdown policy.

## sbt plugin and Laika code

- Keep the plugin opt-in with `noTrigger` unless a future contract explicitly
  requires automatic activation.
- Require only plugins that DocTrellis actually uses.
- Give public settings safe, local-development defaults and prefix them with
  `doctrellis`.
- Keep settings declarative and side-effect free. Put filesystem, rendering,
  server, and process work in tasks.
- Mark tasks uncached only when they observe external state or own a long-lived
  resource that cannot be represented correctly by the task cache.
- Use `streams.value.log`; do not use `println` for task diagnostics.
- Delete or replace generated output deliberately so stale files cannot appear
  in a successful site.
- Serve files only after canonical path-containment checks, with explicit MIME
  types and bounded path decoding.
- Bind local preview servers to loopback by default.

Prefer Laika parser extensions and AST rewrites to post-processing rendered
HTML. Raw Markdown preprocessing is acceptable only when the parser extension
points cannot express the grammar. Such preprocessing must preserve unaffected
input and be covered by regression tests for line endings, indentation, code
blocks, and nested structures.

## Errors and diagnostics

- Use `sys.error` only at an sbt task boundary that must fail the build.
- Error messages must state the operation and relevant field, workspace, path,
  or URL.
- Send user-facing process diagnostics to the DocTrellis output channel or the
  sbt logger, not to arbitrary standard output.

## Project test scope

Apply the base testing principles with these DocTrellis-specific scopes:

- Test DocTrellis opaque parsers, including `WorkingDirectory`, `PreviewUrl`,
  and timing values, with accepted and representative invalid inputs.
- Test application programs with controlled algebra values or in-memory
  interpreters; do not load Node or VS Code to test lifecycle decisions.
- Test `PreviewService` for concurrent start, stop during startup,
  cancellation, process exit, and cleanup failure explicitly.
- Use scripted tests for behavior visible to an external sbt build.
- Use temporary directories and ephemeral ports where possible, and release
  them even when assertions fail.

## Documentation and public contracts

For DocTrellis, the base guide's public-contract synchronization rule covers:

- the Scala domain model and parser;
- `.doctrellis.json` schema;
- VS Code manifest contributions;
- the basic example;
- README usage instructions; and
- tests.

Add or supersede a DocTrellis ADR when one of those changes affects module
boundaries, process ownership, configuration compatibility, toolchain strategy,
or publishing policy.

Do not edit or commit generated directories and packages such as `target/`,
`.bloop/`, `node_modules/`, `dist/`, or `*.vsix`.

## Verification before commit

Format changed Scala and sbt sources:

```console
sbt --batch 'scalafixAll; scalafmtAll; scalafmtSbt'
sbt --batch 'scalafmtCheckAll; scalafmtSbtCheck'
```

Treat Scalafix followed by Scalafmt as one formatting pass: `OrganizeImports`
merges imports without trailing commas, then Scalafmt restores the required
trailing commas. CI runs this pass on a clean checkout and uses
`git diff --exit-code` instead of `scalafixAll --check` to verify that both
policies are satisfied together.

Run checks proportional to the change, and run the full suite before a release
or a cross-module commit:

```console
sbt --batch testFull
sbt --batch 'sbtDoctrellis / scripted'
sbt --batch 'vscode / Compile / fastLinkJS'
sbt --batch 'vscode / Compile / fullLinkJS'
```

For extension packaging changes, also run:

```console
cd modules/vscode
npm ci
npm test
npm run vsix
npm run vsix:list
```

## Project review checklist

Complete the base guide's review checklist, then confirm that:

- the module-to-role mapping and inward dependency direction remain intact;
- Scala.js, Node, VS Code, and mutable lifecycle code stay in the extension
  boundary;
- the Scala model, schema, manifest, examples, and documentation agree;
- sbt plugin behavior visible to consuming builds has scripted coverage; and
- Scalafmt, Scalafix, compiler warnings, WartRemover, tests, and relevant
  packaging checks pass.

## Project-specific references

- [sbt-tpolecat](https://github.com/typelevel/sbt-tpolecat)
- [Scalafix `OrganizeImports`](https://scalacenter.github.io/scalafix/docs/rules/OrganizeImports.html)
- [VS Code Webview API](https://code.visualstudio.com/api/extension-guides/webview)
