# ADR 0001: Repository and functional architecture

- Status: Accepted
- Date: 2026-07-19
- Updated: 2026-07-20

## Context

DocTrellis has two distributable parts: an sbt plugin that owns documentation
generation and a VS Code extension that owns local process UX. Their artifacts
are independent, but their command, URL, schema, and lifecycle contracts evolve
together. Splitting them immediately would require cross-repository coordination
before either artifact has a stable public contract.

The extension also crosses several capability boundaries: pure configuration
rules, concurrent lifecycle decisions, Node process/HTTP access, and VS Code
UI APIs. Mixing those concerns would make lifecycle behavior difficult to test
outside an Extension Development Host.

## Decision

Use one repository with independently named modules:

- `modules/sbt-doctrellis` is a JVM sbt plugin.
- `modules/vscode/domain` contains parsed values and platform-independent data.
- `modules/vscode/application` defines the preview algebra in an abstract
  `F[_]` and composes it into the lifecycle program.
- `modules/vscode/extension` contains Node and VS Code interpreters, the
  composition root, and the exported extension entry points.

The extension uses a tagless-final style: the algebra describes the smallest
capabilities required by the program, interpreters implement those capabilities
using platform APIs, and the composition root selects `IO` and wires the parts
together. The module dependency direction remains inward:

```text
extension (interpreters and composition root)
  -> application (algebra and program)
  -> domain
```

No inner module imports a Node or VS Code facade. The plugin remains a separate
vertical module because sbt tasks and Laika are JVM concerns, not extension
domain concerns.

Apply the Principle of Least Power at boundaries: use a direct executable plus
argument vector instead of a shell, expose only the VS Code/Node members in use,
prefer immutable values, and keep mutation at the extension entry point where
VS Code's lifecycle requires it.

Apply “Parse, don't validate” to configuration: convert untyped JSON into
opaque, constrained domain values once. Application programs receive a
`PreviewCommand` whose executable, working directory, URL, and timeout have
already been parsed.

## Consequences

- A single change can update the plugin, schema, extension, example, and
  integration tests atomically.
- Domain and application tests run under Scala.js without loading VS Code.
- Platform facades and unsafe callbacks stay at the outer edge.
- The repository build is larger than either artifact alone, but subprojects
  preserve separate compilation and eventual release boundaries.
- A later repository split remains possible if the contracts and release
  cadences diverge materially.

## References

- [Strategic Scala Style: Principle of Least Power](https://www.lihaoyi.com/post/StrategicScalaStylePrincipleofLeastPower.html)
- [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
- [sigilaris/sigilaris](https://github.com/sigilaris/sigilaris), used as a style reference for small algebras, resource safety, module boundaries, and WartRemover
