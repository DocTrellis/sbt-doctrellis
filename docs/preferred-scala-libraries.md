# Preferred Scala Libraries

This guide records the principles used to select Scala libraries and a small
set of preferred examples. The examples are starting points, not automatic
dependencies: each project must still confirm that a library fits its use case,
platforms, Scala version, and operational constraints.

## Selection principles

Evaluate libraries in this order:

1. **Correct implementation.** The library must implement its stated semantics
   correctly, including edge cases and resource or concurrency behavior. Active
   maintenance is part of correctness: supported releases must continue to
   receive compatibility, security, and correctness fixes.
2. **Strong type safety.** Prefer APIs that preserve domain guarantees, make
   invalid states difficult to represent, and expose failures and capabilities
   explicitly. Integrations must retain those guarantees at their boundaries.
3. **High performance.** Prefer libraries whose latency, throughput, allocation,
   and resource usage suit the real workload. When performance drives the
   choice, support claims with relevant, reproducible measurements rather than
   reputation alone.

A preferred library must satisfy all three principles. Their order resolves
trade-offs: performance does not compensate for incorrect behavior, inactive
maintenance, or the loss of important type guarantees.

Before adoption or a major upgrade:

- check the supported Scala versions and target platforms;
- review recent releases, open correctness issues, tests, and compatibility
  policy;
- benchmark behavior that matters to the project when performance affects the
  choice; and
- depend only on the modules the project needs and select their versions
  centrally.

Re-evaluate a preferred library when its maintenance status, API guarantees,
performance characteristics, or the project's requirements change.

## Preferred examples

These examples are intentionally not exhaustive:

| Library | Typical fit |
| --- | --- |
| [Cats Effect](https://typelevel.org/cats-effect/) (`cats-effect`) | Typed effects, concurrency, cancellation, and resource safety |
| [Tapir with Armeria](https://tapir.softwaremill.com/en/latest/server/armeria.html) (`tapir-armeria-server-cats`) | Typed HTTP endpoint contracts served through the Armeria Cats Effect interpreter; clients use the sttp v4 interpreter with an Armeria backend as defined by the [Tapir HTTP contract guide](tapir-http-contracts.md) |
| [Scribe](https://github.com/outr/scribe) (`scribe`, `scribe-slf4j`) | Scala logging, with the SLF4J bridge added only when interoperability requires it |
| [Tyrian](https://tyrian.indigoengine.io/) (`tyrian`) | Elm-inspired Scala 3 web user interfaces on Scala.js |
| [Iron](https://iltotore.github.io/iron/) (`iron`) | Reusable and composable refined type constraints |

Being listed here does not require using a library when the standard library or
a small local abstraction solves the problem more clearly. Record a
project-specific exception when another library fits the selection principles
and the project's requirements better.
