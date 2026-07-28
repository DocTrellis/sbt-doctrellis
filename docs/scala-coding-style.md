# Scala 3 Functional Coding Style Guide

This guide defines reusable coding conventions for Scala 3 projects using sbt
and the Typelevel ecosystem, especially Cats Effect. A project may add a
project-specific guide that maps these principles to its modules, platforms,
tool configuration, and verification commands.

The goal is not merely uniform formatting. Code should make invalid states
hard to represent, keep effects and platform capabilities at explicit
boundaries, and use the least powerful construct that solves the problem.

The words **must**, **should**, and **may** indicate required, recommended, and
optional practices respectively.

Automated formatter, compiler, and lint configuration is the source of truth
for mechanical rules. This guide covers design and review choices that those
tools cannot enforce. When they appear to disagree, clarify the rule or update
the relevant decision explicitly; do not work around the automated check.

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
  to concrete `IO` in programs and algebras.
- Prefer a small record of algebra operations to a deep trait hierarchy.
- Prefer an opaque type and a smart constructor to a broadly constructible
  wrapper.
- Prefer an enum or a sealed error hierarchy to strings and booleans that
  encode states indirectly.
- Prefer direct process execution with an argument vector to shell command
  construction.
- Prefer a hand-written platform binding containing only used members to a
  generated binding for an entire API.
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
def parse(value: String): Either[ParseProblem, ServiceUrl]

final case class ServiceAlgebra[F[_]](
    probe: ServiceUrl => F[Boolean],
)
```

Avoid:

```scala
def isValidUrl(value: String): Boolean

final case class ServiceAlgebra(
    probe: String => IO[Boolean],
)
```

Apply these rules consistently:

- Put smart constructors in the companion object of the parsed type.
- Keep opaque type constructors inaccessible outside their defining scope.
- Represent malformed boundary input with a structured error that identifies
  the offending field or boundary; do not throw from a domain parser.
- Parse versioned configuration and reject unknown fields before starting any
  effect.
- Resolve and canonicalize filesystem paths before constructing a domain path
  whose type promises those properties.
- Make programs accept parsed domain values, not platform objects, decoded JSON
  trees, arbitrary strings, or unresolved paths.
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
- Do not add `require` calls to case classes when a parser can make invalid
  construction impossible.

A boundary may have more than one narrowing stage. For example, decoding may
produce a command template, while filesystem resolution produces a command
that is safe to execute. Each stage must narrow the possible states.

## Functional architecture: algebra, program, and interpreter

This guide uses a tagless-final style with compile-time dependencies directed
toward domain concepts. Algebras describe capabilities in an abstract `F[_]`,
programs compose those capabilities, and interpreters connect them to external
systems. The outer composition root selects the concrete effect and wires the
parts together.

```text
compile-time dependency direction

composition root
  |-> program -> algebra -> domain
  `-> interpreters
        |-> algebra
        |-> domain
        `-> platform APIs
```

This vocabulary describes roles rather than requiring one package, trait, or
type class per role:

- **Domain** owns parsed values, state, and deterministic pure rules. It does
  not mention `F[_]` or platform types.
- **Algebra** describes the smallest capabilities a program requires in terms
  of domain inputs and outputs. A small record of functions such as
  `ServiceAlgebra[F]` is an algebra; do not introduce a trait or type class when
  a record is sufficient.
- **Program** implements use cases and lifecycle orchestration using only
  domain values, algebras, and the weakest effect capabilities it needs. It
  remains polymorphic in `F[_]` and must not select `IO` for itself.
- **Interpreter** implements an algebra using an external system. It translates
  platform values at the boundary so they do not escape into the program or
  domain.
- **Composition root** acquires shared resources, selects the concrete effect,
  constructs interpreters, and supplies them to programs.

In this guide, an *interpreter* is a production or test implementation of an
algebra; `IO` and `SyncIO` are concrete effect types, not interpreters.

Apply these dependency rules:

- Programs may depend on algebras and the domain; interpreters may depend on
  algebras, the domain, and their platform APIs.
- Inner modules must not import outer-module packages.
- Algebra operations must accept and return parsed domain values rather than
  raw platform representations.
- Keep each algebra no more powerful than the program that consumes it. Split
  unrelated capabilities when doing so makes ownership or reuse clearer, but
  do not split a cohesive record merely to create more abstractions.
- Tests should supply controlled algebra functions or in-memory interpreters;
  they should not require changes to program logic.
- Domain code must remain deterministic and independently testable.

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
final class SessionProgram[F[_]: Concurrent](
    sessions: Ref[F, Map[Workspace, Session[F]]],
    mutex: Mutex[F],
):
  def start(workspace: Workspace): F[Unit] =
    mutex.lock.surround:
      sessions.get.flatMap:
        case sessions if sessions.contains(workspace) =>
          Concurrent[F].unit
        case _ => create(workspace)
```

Avoid adding braces around declarations and callbacks that have a direct
indentation-based form.

### Compiler warnings

The default build mode should treat compiler warnings as errors so production
and test code remain warning free.

- Configure warning policy centrally through the build's selected compiler
  policy mechanism; do not add scattered `scalacOptions` exceptions.
- Use the project's documented development mode for temporary local iteration
  without fatal warnings; do not edit committed build settings to switch modes.
- Scope exclusions as narrowly as possible and document why the excluded
  warning cannot be fixed at its source.
- Do not exclude a warning globally merely to accommodate one test, generated
  source, REPL, or platform boundary.

### Import organization

Imports must follow one deterministic layout. Group them in this order, with a
blank line between non-empty groups:

1. `java` and `javax` packages
2. `scala` packages
3. `cats` packages
4. Other third-party packages
5. Project packages

Merge imports that share a prefix and sort selectors by ASCII order. Sort
import statements with `SymbolsFirst`: wildcard and grouped imports precede
ordinary imports sharing the same prefix, while other path segments remain in
lexicographic order. Do not turn a large selector list into a wildcard
automatically; wildcard imports are still limited to the DSL cases described
above. Removing unused imports is a separate compiler or lint concern.

Enforce this policy with the project's configured import-organization tool.

## Types, data, and naming

- Use `final` for production classes and case classes unless inheritance is a
  deliberate part of the model.
- Use enums for closed state machines and alternatives that carry domain data.
- Use sealed exception hierarchies when failures must also be `Throwable`.
- Use case classes instead of public tuples when fields have domain meaning.
- Use named arguments when a constructor has several parameters of the same
  type or multiple function-valued fields, especially when assembling algebra
  records.
- Use opaque types for scalar values whose invariants are established during
  parsing and that do not need product structure.
- When scalar constraints must be reused or composed broadly enough that local
  opaque types would duplicate parser and smart-constructor logic, consider a
  refined-type library such as Iron. Do not add such a dependency for an
  isolated invariant that an opaque type can express clearly.
- Name opaque type constructors `parse` when they consume untrusted input.
- Expose the underlying scalar with a narrowly scoped extension such as
  `.value` or `.duration`, primarily for interpreters.
- Name effects with verbs such as `start`, `stop`, `load`, `probe`, and
  `render`.
- Name domain values and states with nouns.
- Name failures after the condition rather than the place where they were
  detected.
- Use `private` or `private[package]` aggressively. Public API is a maintenance
  commitment.

Do not use `null` in Scala code. At foreign-platform boundaries, test null-like
values explicitly and translate them immediately into Scala values.

Avoid partial operations, including `Option.get`, `Try.get`, unsafe either
projections, unchecked casts in inner modules, and non-exhaustive matches.
Automated lint may enforce part of this rule; review must cover the rest.

## Cats Effect and effect polymorphism

Programs, their algebras, and supporting resources should be parameterized by
`F[_]`. The composition root selects a concrete effect. Keep effects visible in
signatures and follow these rules:

- A data-only algebra such as `ServiceAlgebra[F]` should not impose a type class
  constraint. Put constraints on the programs or interpreter implementations
  that use or construct it.
- Use `Functor` or `Monad` for pure effect composition, `MonadThrow` for raised
  errors, and `MonadCancelThrow` for cancellation-aware resource scopes.
- Add `Sync` only when suspending synchronous side effects, `Temporal` only
  when time and sleeping are required, `Concurrent` only for concurrent state
  and fibers, and `Async` only for asynchronous callback or promise bridges.
- If one implementation needs a stronger constraint, keep that constraint on
  the implementation rather than widening the algebra or every caller.
- Wrap synchronous effects with `Sync[F].delay`; use `Sync[F].blocking` for
  blocking JVM work.
- Convert foreign promises and callbacks with `Async[F]` at the interpreter
  boundary.
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

Concrete `IO`, `IORuntime`, unsafe runners, promise conversion, and dispatcher
unsafe methods are allowed only at an outer runtime boundary, such as:

- an executable or framework entry point that must return a host value;
- a host callback being transferred into the selected effect runtime; or
- an integration boundary that must synchronously return to external code.

Do not start raw threads or futures when a Cats Effect primitive expresses the
same ownership and cancellation semantics.

## Shared HTTP API contracts

These rules apply to structured request/response HTTP APIs whose server and
client contracts are owned together. They do not require an endpoint library
for static file serving, a single-purpose local health or callback endpoint, or
non-HTTP IPC. A project-specific guide or ADR must define the precise scope and
select any server and client implementations.

When a project uses Tapir, define the transport contract once as shared
`Endpoint` values. Derive server endpoints, client requests, and API
documentation from those same values; do not independently repeat methods,
paths, parameters, headers, bodies, status codes, or codecs.

Treat an endpoint description as a transport contract, not a domain algebra or
program:

- Keep Tapir out of domain and program layers. A shared contract module may own
  endpoint descriptions, transport models, API error ADTs, codecs, schemas, and
  metadata, but not server logic or interpreter selection.
- Share `Endpoint` or `PublicEndpoint` descriptions, not `ServerEndpoint`
  values with attached logic.
- Give each endpoint a stable, unique name and export one complete endpoint
  list for serving, documentation, and verification.
- Translate transport models into parsed domain values before invoking a
  program, and translate decoded client responses before returning them from a
  domain algebra.
- Prefer codecs that parse path, query, and header inputs into domain types
  that encode the parsed guarantee. Validation that returns the original raw
  type does not establish a guarantee for inner code.
- Keep effectful or state-dependent checks in programs. Represent expected
  rejections with a closed API error ADT and describe every public status and
  body variant in the endpoint.
- Define credentials as security inputs and use the client interpreter's secure
  path for secure endpoints.
- Derive schemas explicitly at the contract boundary. Avoid broad automatic
  derivation imports that can silently change a public schema after an
  unrelated model edit.
- Generate OpenAPI from the exported endpoint list. If the server and client
  cannot share a Scala artifact, publish that document as the exchange contract
  and verify both sides against it.

Run `EndpointVerifier` over the complete endpoint list, exercise interpreted
clients against `ServerEndpoint` logic without a network where possible, and
pin externally observable requests and responses with integration tests. A
round trip derived from one shared endpoint cannot detect an unintended
simultaneous contract change, so published APIs also require compatibility
checks against their released OpenAPI baseline.

## Errors and logging

- Domain parsing failures should be structured values.
- Interpreter and lifecycle failures may be exceptions inside `F[_]`.
- Error messages must state the operation and relevant domain context.
- Preserve the original exception as the cause when adding context.
- Catch `NonFatal` on the JVM unless the boundary requires a narrower type.
- Never catch `Throwable` merely to keep execution moving.
- Do not discard errors unless an operation is explicitly best-effort, such as
  secondary logging or cleanup after the primary outcome is already known.
- Send user-facing diagnostics to the application's designated logger or
  output channel, not to arbitrary standard output.

## Tests

Every behavior change must have a test at the narrowest scope that can prove it.

Use MUnit for example-based tests.

- Test parsed types with accepted boundary values and representative invalid
  values.
- Test programs with controlled algebra values or in-memory interpreters.
- Test concurrent startup, interruption, cancellation, exit, and cleanup
  failure explicitly when the lifecycle supports those states.
- Prefer `Deferred`, controlled probes, and deterministic fakes to arbitrary
  sleeps.
- Use bounded timeouts only where the real clock or operating system is part of
  the behavior.
- Test interpreters for malformed platform values and resource cleanup.
- Use temporary resources where possible, and release them even when assertions
  fail.
- Give tests behavior-oriented names that explain the expected outcome.

A bug fix should include a regression test that fails for the original bug.

### Property-based testing

Use property-based tests when behavior can be expressed as a law over a broad
input space. They complement example-based tests; retain focused examples for
boundary cases, regressions, and executable documentation.

Prefer `scala-hedgehog` when introducing property-based tests. Its integrated
shrinking keeps generated values and their shrunk counterexamples within the
same construction rules, and its state-machine support fits lifecycle and
resource-management testing. Use another property-testing library only when a
concrete platform or integration constraint outweighs those benefits, and
record the reason near the build dependency.

Integrate Hedgehog through `hedgehog-munit` so property-based and example-based
suites use the MUnit test framework. Do not add the separate Hedgehog sbt test
framework unless a concrete integration constraint requires it.

- State the property in domain terms before choosing generators.
- Prefer round trips for encoders, decoders, and reversible transformations.
- Compare stateful services with a small, pure reference model by generating
  sequences of commands and checking their postconditions.
- Check invariants preserved by transformations, including content, size,
  containment, ordering, and resource ownership where applicable.
- Check idempotence for normalization, stopping, cleanup, and other operations
  whose contract says repeated application has no additional effect.
- Check that distinct paths which the contract says are equivalent reach the
  same result. Test commutativity, associativity, and type class laws only when
  each law is part of the public contract.
- Use structural properties for recursive data and black-box oracles when
  producing a result is difficult but independently checking it is simple.
- Generate valid domain values by construction. Generate malformed raw values
  separately when testing parsers and other boundaries.
- Keep generators and reference models simpler than the production
  implementation; duplicating its algorithm proves little.
- Choose explicit ranges and classifications that cover empty, boundary,
  ordinary, and large cases relevant to the property. Avoid relying on
  filtering when a generator can construct the intended domain directly.
- Turn the smallest counterexample reported by Hedgehog into a focused
  regression test. Record the seed reported in the failure output and reproduce
  the generated run by setting that value through the `HEDGEHOG_SEED`
  environment variable.
- For effectful or concurrent code, generate operation sequences against a
  deterministic model.

### Concrete effects in example-based tests

When an example-based test must execute effects or manage a `Resource`, use
MUnit Cats Effect and let the test return its effect directly; do not call
`unsafeRunSync`.

- Choose the least powerful concrete test effect that can exercise the
  behavior. When an effect-polymorphic implementation requires only synchronous
  effects, instantiate it with `SyncIO`. This verifies that the implementation
  and its algebra dependencies are not accidentally coupled to `IO`.
- Use `IO` when the behavior involves concurrency, fibers, cancellation,
  scheduling, time, asynchronous callbacks, or runtime-specific blocking
  semantics. A `SyncIO` test can verify ordinary and failed resource release,
  but it cannot prove cancellation or concurrent lifecycle behavior.
- Do not repeat the same test with both `SyncIO` and `IO` unless
  concrete-effect-specific behavior is part of the contract.

## Documentation and contracts

When a public contract changes, update all affected representations in the same
change, including applicable models, parsers, schemas, manifests, examples,
usage documentation, and tests.

Record changes to module boundaries, resource ownership, configuration
compatibility, toolchain strategy, or publishing policy in the project's
architectural decision records. Keep examples executable; documentation
snippets are part of the product contract.

Do not edit or commit generated outputs. The project-specific guide should name
the directories and package artifacts that this rule covers.

## Verification and commits

- Run configured formatting and linting tools over changed Scala and sbt
  sources.
- Run checks proportional to the change, and run the full suite before a
  release or a cross-module commit.
- Verify generated or packaged artifacts when their inputs or build process
  change.
- Keep commits focused and use an English imperative subject.
- Include a commit body that explains material architectural or behavioral
  changes.
- Commit only from a state in which the relevant checks pass.

## Review checklist

Before requesting review, confirm that:

- raw input is parsed once into a type that represents its guarantees, and
  inner functions do not reintroduce the uncertainty eliminated by parsing;
- dependencies still point toward the domain;
- structured HTTP server, client, and documentation representations derive from
  one contract where the project owns them together;
- new capabilities are no broader than the program needs;
- acquisition, cancellation, failure, and release paths are all explicit;
- platform-specific and mutable code stays at the outer boundary;
- significant indentation is used wherever it improves clarity;
- public contracts, examples, and documentation agree;
- regression and lifecycle tests cover the meaningful failure modes; and
- formatting, compiler warnings, linting, tests, and relevant packaging checks
  pass.

## References

- [Strategic Scala Style: Principle of Least Power](https://www.lihaoyi.com/post/StrategicScalaStylePrincipleofLeastPower.html)
- [Parse, don't validate](https://lexi-lambda.github.io/blog/2019/11/05/parse-don-t-validate/)
- [Iron: refined types for Scala](https://github.com/Iltotore/iron)
- [Cats Effect documentation](https://typelevel.org/cats-effect/docs/getting-started)
- [Tapir documentation](https://tapir.softwaremill.com/en/latest/)
- [Tapir testing and contract verification](https://tapir.softwaremill.com/en/latest/testing.html)
- [MUnit Cats Effect](https://github.com/typelevel/munit-cats-effect)
- [Hedgehog for Scala](https://hedgehogqa.github.io/scala-hedgehog/)
- [Hedgehog integration with MUnit](https://hedgehogqa.github.io/scala-hedgehog/docs/integration-munit/)
- [스칼라에서 속성 기반 테스트로 블록체인 개발하기](https://github.com/sungkmi/liftio-2022-slides/blob/main/liftio-2022-slides.md)
- [sigilaris/sigilaris](https://github.com/sigilaris/sigilaris)
