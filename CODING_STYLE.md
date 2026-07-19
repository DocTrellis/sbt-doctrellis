# DocTrellis Coding Style Guide

This guide defines the coding conventions for DocTrellis. It applies to the
sbt plugin, the Scala.js VS Code extension, tests, build definitions, and
supporting scripts.

The goal is not merely uniform formatting. Code should make invalid states
hard to represent, keep effects and platform capabilities at explicit
boundaries, and use the least powerful construct that solves the problem.

The words **must**, **should**, and **may** indicate required, recommended, and
optional practices respectively.

## Sources of truth

- `.scalafmt.conf` is the source of truth for mechanical Scala formatting.
- `build.sbt` is the source of truth for enabled WartRemover checks.
- The accepted ADRs under `docs/adr/` are the source of truth for architectural
  decisions.
- This guide covers design and review choices that formatters and linters
  cannot enforce.

When these sources appear to disagree, do not work around an automated check.
Clarify the rule or update the relevant decision explicitly.

## Design priorities

Use this order when two style rules compete:

1. Correctness and resource safety
2. Valid data at architectural boundaries
3. Clear dependency direction
4. The Principle of Least Power
5. Readability and consistency
6. Concision

Significant indentation often makes Scala 3 code shorter, but reducing the
line count must never hide lifecycle behavior, error handling, or ownership.

## Principle of Least Power

Choose the least expressive mechanism that fully represents the requirement:

- Prefer immutable `val` bindings to mutable state.
- Prefer a pure function to an effectful function.
- Prefer an abstract `F[_]` with the smallest required capability constraint
  to concrete `IO` in inner layers.
- Prefer a small record of port functions to a deep trait hierarchy.
- Prefer an opaque type and a smart constructor to a broadly constructible
  wrapper.
- Prefer an enum or a sealed error hierarchy to strings and booleans that
  encode states indirectly.
- Prefer direct process execution with an argument vector to shell command
  construction.
- Prefer a hand-written facade containing only used members to a generated
  platform binding.
- Prefer a local value or helper to a new public abstraction used only once.

Treat every type class constraint as a capability grant. Add only the weakest
constraint needed by the implementation, and do not require `Async` merely for
convenience when `Functor`, `MonadThrow`, `MonadCancelThrow`, `Temporal`,
`Concurrent`, or `Sync` is sufficient. Effect polymorphism is primarily a way
to restrict what inner code can do; testability is a useful consequence.

Macros, reflection, unchecked casts, and global registries grant much broader
power and require a concrete justification at a platform boundary.

## Parse, do not validate

Untrusted values must be converted into domain values at the boundary. Do not
check a raw value and continue passing the same raw type through the system.

Parsing allows code inside the boundary to be composed from total functions.
Prefer strengthening the input type to weakening the result type. When parsing
can establish a guarantee, encode it in the domain value so an inner function
can return an ordinary value instead of repeating the check or propagating an
unnecessary `Option` or `Either`.

Prefer:

```scala
def parse(value: String): Either[ConfigProblem, PreviewUrl]

final case class PreviewPorts[F[_]](
    probe: PreviewUrl => F[Boolean],
)
```

Avoid:

```scala
def isValidUrl(value: String): Boolean

final case class PreviewPorts(
    probe: String => IO[Boolean],
)
```

Apply these rules consistently:

- Put smart constructors in the companion object of the parsed type.
- Keep opaque type constructors inaccessible outside their defining scope.
- Represent malformed boundary input with a structured error that identifies
  the offending field or boundary; do not throw from a domain parser.
- Parse configuration versions and reject unknown fields before starting any
  effect.
- Resolve and canonicalize filesystem paths before constructing a
  `WorkingDirectory`.
- Make application services accept parsed domain values, not `js.Any`, JSON
  objects, arbitrary strings, or unresolved paths.
- Prefer `A => B` to `A => Option[B]` or `A => Either[E, B]` when the
  guarantees encoded by `A` make `B` inevitable.
- Use `Option`, `Either`, or a domain-specific result ADT inside the boundary
  only when absence, rejection, or failure remains a legitimate outcome for an
  already parsed domain input.
- Do not invent fallback values or rely on "impossible" exceptions merely to
  make a function appear total. Instead, refine the input type when it can rule
  out the case.
- Keep environmental and effect failures explicit in `F[_]`; they are distinct
  from malformed boundary input and cannot be eliminated by parsing.
- Do not add `require` calls to case classes when a parser can make the invalid
  construction impossible.

A multi-stage boundary is acceptable. For example, JSON parsing produces a
`PreviewTemplate`; filesystem resolution then produces a `PreviewCommand`.
Each stage must narrow the possible states.

## Onion architecture

Dependencies point inward:

```text
extension adapters -> application -> domain
```

The sbt plugin is a separate JVM vertical module because sbt, mdoc, Laika, and
the preview HTTP server are not VS Code domain concerns.

| Layer | Owns | Must not depend on |
| --- | --- | --- |
| `modules/vscode/domain` | Parsed values, state, and pure rules | Cats Effect, Node, VS Code, sbt |
| `modules/vscode/application` | Use cases, lifecycle orchestration, and ports | Node and VS Code facades |
| `modules/vscode/extension` | Node/VS Code adapters and exported entry points | Inner-layer implementation details |
| `modules/sbt-doctrellis` | Documentation build and local HTTP preview | VS Code extension modules |

Additional rules:

- Inner layers must not import outer-layer packages.
- Platform types must be translated at the adapter boundary.
- A port should describe the smallest capability required by a use case.
- Domain code must remain deterministic and independently testable.
- Cross-cutting changes to this dependency direction require an ADR.

## Scala 3 syntax and formatting

Run Scalafmt instead of manually aligning code. Use Scala 3 syntax throughout.

- Use significant indentation and `:` syntax whenever the construct supports
  it clearly.
- Use braces only where required by syntax, interpolation, imports, macros, or
  platform interoperability, or where significant indentation would be
  genuinely ambiguous.
- Do not use semicolons.
- Use `then`, `do`, `match`, and indentation-based lambdas consistently.
- Use `as` for renamed imports and `*` for wildcard imports.
- Keep wildcard imports limited to established DSLs such as sbt keys, Cats
  syntax, or a deliberately small project package.
- Include trailing commas in multiline argument and parameter lists.
- Give public and protected definitions explicit result types.
- Let private local values infer their types when the result is obvious.
- Keep one primary top-level production definition per file unless tightly
  coupled private definitions are clearer beside it.

Prefer:

```scala
final class PreviewService[F[_]: Concurrent](
    sessions: Ref[F, Map[String, PreviewSession[F]]],
    mutex: Mutex[F],
):
  def start(workspace: Workspace): F[Unit] =
    mutex.lock.surround:
      sessions.get.flatMap:
        case sessions if sessions.contains(workspace.key) =>
          Concurrent[F].unit
        case _ => create(workspace)
```

Avoid adding braces around declarations and callbacks that have a direct
indentation-based form.

## Types, data, and naming

- Use `final` for production classes and case classes unless inheritance is a
  deliberate part of the model.
- Use enums for closed state machines and alternatives that carry domain data.
- Use sealed exception hierarchies when failures must also be `Throwable`.
- Use case classes instead of public tuples when fields have domain meaning.
- Use named arguments when a constructor has several parameters of the same
  type or multiple function-valued fields, especially when assembling ports.
- Use opaque types for scalar values whose invariants are established during
  parsing and that do not need product structure.
- When scalar constraints must be reused or composed broadly enough that local
  opaque types would duplicate parser and smart-constructor logic, consider a
  refined-type library such as Iron. Do not add such a dependency for an
  isolated invariant that an opaque type can express clearly.
- Name opaque type constructors `parse` when they consume untrusted input.
- Expose the underlying scalar with a narrowly scoped extension such as
  `.value` or `.duration`, primarily for adapters.
- Name effects with verbs such as `start`, `stop`, `load`, `probe`, and
  `render`.
- Name domain values and states with nouns such as `PreviewCommand` and
  `PreviewStatus`.
- Name failures after the condition, for example `PreviewTimedOut`, rather
  than after the place where they were detected.
- Use `private` or `private[package]` aggressively. Public API is a maintenance
  commitment.

Do not use `null` in Scala code. At JavaScript boundaries, test `null` and
`undefined` explicitly and translate them immediately into Scala values.

Avoid partial operations, including `Option.get`, `Try.get`, unsafe either
projections, unchecked casts in inner layers, and non-exhaustive matches.
WartRemover enforces part of this rule; review must cover the rest.

## Cats Effect and effect polymorphism

Application services, ports, and their supporting resources should be
parameterized by `F[_]`. The outer runtime selects `IO` as the interpreter.
Keep effects visible in signatures and follow these rules:

- A data-only algebra such as `PreviewPorts[F]` should not impose a type class
  constraint. Put constraints on the code that interprets or combines its
  operations.
- Use `Functor` or `Monad` for pure effect composition, `MonadThrow` for raised
  errors, and `MonadCancelThrow` for cancellation-aware resource scopes.
- Add `Sync` only when suspending synchronous side effects, `Temporal` only
  when time and sleeping are required, `Concurrent` only for concurrent state
  and fibers, and `Async` only for asynchronous callback or promise bridges.
- If one implementation needs a stronger constraint, keep that constraint on
  the implementation rather than widening the algebra or every caller.
- Wrap synchronous effects with `Sync[F].delay`; use `Sync[F].blocking` for
  blocking JVM work.
- Convert JavaScript promises and callbacks with `Async[F]` at the
  infrastructure boundary.
- Represent owned resources with `Resource[F, A]` and release them in reverse
  acquisition order.
- Make finalizers idempotent whenever more than one lifecycle path can reach
  them.
- Use `Ref[F, A]` for shared state, `Deferred[F, A]` for one-time signals,
  `Mutex[F]` for serialized transitions, and `Supervisor[F]` for owned
  background fibers.
- Keep `uncancelable` regions as small as possible. Restore cancellation around
  waits when atomic installation does not require masking them.
- Specify the behavior for cancellation, startup failure, normal exit, and
  shutdown timeout for every long-lived process.
- Attempt all independent cleanup actions. Do not let one failed finalizer
  silently prevent the remaining cleanup.
- Preserve the primary failure when cleanup also fails, and report cleanup
  failures with enough context to diagnose them.

Concrete `IO`, `IORuntime`, `unsafeRunSync`, `unsafeToPromise`, and `Dispatcher`
unsafe methods are allowed only at an outer runtime boundary:

- an sbt task that must return synchronously;
- an exported VS Code activation or deactivation function; or
- a Node/VS Code callback being transferred into the selected `IO` runtime.

Do not start raw threads or futures when a Cats Effect primitive expresses the
same ownership and cancellation semantics.

## Scala.js and VS Code boundaries

- Keep facades in `modules/vscode/extension/.../facade`.
- Declare only the JavaScript members that production code calls.
- Keep `js.Dynamic`, `js.Any`, casts, `null`, and `undefined` inside the
  infrastructure layer.
- A facade declaration contains no business logic.
- Keep the `vscode` module external; the VS Code extension host provides it.
- Export only the required CommonJS entry points.
- Interpret generic application effects as `IO` at extension assembly, then
  transfer host callbacks through the owned `Dispatcher[IO]`.
- Restrict mutable lifecycle handles to the extension entry point or a tightly
  scoped adapter when the host API requires mutation.
- Resolve remote preview URLs through the supported VS Code URI APIs before
  displaying them.
- Treat workspace configuration as executable authority. Process-starting
  commands must require Workspace Trust at both manifest and runtime layers.

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

## Errors and logging

- Domain parsing failures should be structured values such as `ConfigProblem`.
- Infrastructure and lifecycle failures may be exceptions inside `F[_]`.
- Use `sys.error` only at an sbt task boundary that must fail the build.
- Error messages must state the operation and relevant field, workspace, path,
  or URL.
- Preserve the original exception as the cause when adding context.
- Catch `NonFatal` on the JVM unless the boundary requires a narrower type.
- Never catch `Throwable` merely to keep execution moving.
- Do not discard errors unless an operation is explicitly best-effort, such as
  secondary logging or cleanup after the primary outcome is already known.
- Send user-facing process diagnostics to the DocTrellis output channel or the
  sbt logger, not to arbitrary standard output.

## Tests

Every behavior change must have a test at the lowest layer that can prove it.

- Use MUnit for pure and JVM tests and MUnit Cats Effect with `IO` as the test
  interpreter for effect-polymorphic programs.
- Test opaque parsers with accepted boundary values and representative invalid
  values.
- Test application services through fake ports; do not load Node or VS Code to
  test lifecycle decisions.
- Test concurrent start, stop during startup, cancellation, process exit, and
  cleanup failure explicitly.
- Prefer `Deferred`, controlled probes, and deterministic fakes to arbitrary
  sleeps.
- Use bounded timeouts only where the real clock or operating system is part of
  the behavior.
- Test adapters for malformed platform values and resource cleanup.
- Use scripted tests for behavior visible to an external sbt build.
- Use temporary directories and ephemeral ports where possible, and release
  them even when assertions fail.
- Give tests behavior-oriented names that explain the expected outcome.

A bug fix should include a regression test that fails for the original bug.
Production WartRemover rules also apply as review rules in test code, even when
the test configuration does not enforce every wart mechanically.

## Documentation and contracts

When a public contract changes, update all affected representations in the same
change:

- the Scala domain model and parser;
- `.doctrellis.json` schema;
- VS Code manifest contributions;
- the basic example;
- README usage instructions; and
- tests.

Add or supersede an ADR for changes to module boundaries, process ownership,
configuration compatibility, toolchain strategy, or publishing policy. Keep
examples executable; documentation snippets are part of the product contract.

Do not edit or commit generated directories and packages such as `target/`,
`.bloop/`, `node_modules/`, `dist/`, or `*.vsix`.

## Verification before commit

Format changed Scala and sbt sources:

```console
sbt --batch 'scalafmtAll; scalafmtSbt'
sbt --batch 'scalafmtCheckAll; scalafmtSbtCheck'
```

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

Commits should be focused, use an English imperative subject, and include a
body that explains material architectural or behavioral changes. Commit only
from a state in which the relevant tests pass.

## Review checklist

Before requesting review, confirm that:

- raw input is parsed once into a type that represents its guarantees, and
  inner functions do not reintroduce the uncertainty eliminated by parsing;
- dependencies still point inward;
- new capabilities are no broader than the use case needs;
- acquisition, cancellation, failure, and release paths are all explicit;
- platform-specific and mutable code stays at the outer boundary;
- significant indentation is used wherever it improves clarity;
- public contracts, schema, examples, and documentation agree;
- regression and lifecycle tests cover the meaningful failure modes; and
- formatting, WartRemover, tests, and relevant packaging checks pass.

## References

- [Strategic Scala Style: Principle of Least Power](https://www.lihaoyi.com/post/StrategicScalaStylePrincipleofLeastPower.html)
- [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
- [Iron: refined types for Scala](https://github.com/Iltotore/iron)
- [Cats Effect documentation](https://typelevel.org/cats-effect/docs/getting-started)
- [VS Code Webview API](https://code.visualstudio.com/api/extension-guides/webview)
- [sigilaris/sigilaris](https://github.com/sigilaris/sigilaris)
