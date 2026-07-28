# ADR 0009: Tapir contracts with Armeria for structured HTTP APIs

- Status: Accepted
- Date: 2026-07-27
- Related: ADR 0005

## Context

A structured HTTP API owned on both its Scala server and client sides can drift
when methods, paths, inputs, outputs, errors, and codecs are declared
independently. Tapir can represent that transport contract as values and derive
server, client, and OpenAPI representations from the same definitions.

At acceptance, this decision was based on Tapir 1.13.29, sttp client 4.0.26,
and sttp-apispec 0.11.10. The repository did not yet declare those dependencies,
so its normal build did not guard the capability and packaging claims. The
reusable guide records the
[current version basis](../tapir-http-contracts.md#adoption-and-version-basis);
revalidate the full integration when adding or upgrading the libraries.

The repository also contains two simpler boundaries with different needs. The
sbt plugin serves static preview content with the JDK `HttpServer`, while ADR
0005 defines the preview control channel as framed NDJSON over Unix domain
sockets or Windows loopback TCP. Applying a full HTTP API stack to either
boundary would add dependencies and change an accepted protocol without solving
an existing contract-drift problem.

## Decision

When DocTrellis introduces a structured request/response HTTP API whose Scala
server and client contracts it owns together:

- shared Tapir `Endpoint` values are the transport source of truth;
- the server uses `ArmeriaCatsServerInterpreter`;
- JVM clients use the Tapir sttp v4 client interpreter with
  `ArmeriaCatsBackend`, or `ArmeriaFs2Backend` for streaming;
- non-JVM clients interpret the same endpoint values with a platform-supported
  backend; and
- OpenAPI is generated from the same complete endpoint list.

This decision does not apply to static file serving, single-purpose local
endpoints, or non-HTTP IPC. The project guide's
[Structured HTTP APIs](../coding-style.md#structured-http-apis) section owns the
precise boundary list and application conditions.

The Armeria server and JVM client lifecycles are resources owned by their
composition roots. Their `Async` requirement does not propagate into domain
algebras or programs.

The selected Tapir Armeria Cats Effect server interpreter and the sttp Armeria
backends do not provide an end-to-end Tapir WebSocket path. A WebSocket API must
supersede this decision or record a narrow exception.

## Unix domain sockets

Armeria makes Unix domain sockets feasible for a future JVM-to-JVM local HTTP
API, subject to pinned-version integration tests, native-transport availability,
socket-path security, and a Windows loopback fallback.

The existing preview control channel will not be migrated merely to reuse
Armeria. Its client runs as Scala.js on Node and cannot use the JVM Armeria
backend, while its persistent NDJSON conversation and ownership protocol are
not equivalent to independent HTTP requests. Such a change would additionally
need compatible native transports, socket ownership and stale-file cleanup,
filesystem permission rules, a Windows fallback, and a migration plan for the
persistent stream semantics. It requires a new protocol version and an ADR that
amends ADR 0005.

## Consequences

- A structured API has one executable server/client/documentation contract.
- Armeria and sttp dependencies enter only modules that implement such an API.
- Static preview serving and workspace IPC do not acquire those dependencies.
- Server and client Armeria transitive versions require convergence checks.
- Capability and packaging claims follow the reusable guide's version basis,
  not timeless properties; upgrades must re-check them.
- Unix domain sockets remain available for a suitable JVM-local HTTP API, but
  are not a reason to rewrite the current Node/JVM control protocol.
- Tapir or Armeria features outside the selected interpreters' capabilities
  require an explicit architecture review.

Implementation and verification rules are in the
[Tapir HTTP Contract Guide](../tapir-http-contracts.md).
