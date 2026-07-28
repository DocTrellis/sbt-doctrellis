# Tapir HTTP Contract Guide

This guide applies the reusable
[Scala HTTP contract rules](scala-coding-style.md#shared-http-api-contracts)
with Tapir, Armeria, and sttp. It defines a fixed implementation stack for a
structured request/response HTTP API whose Scala server and client are owned
together.

## Adoption and version basis

Copy this guide together with `scala-coding-style.md`. It depends on that base
guide's `shared-http-api-contracts` anchor; if either file or heading is
renamed, update both relative links in this document.

This guide does not decide which application APIs adopt the stack or whether
an existing server or IPC transport should be replaced. Record project-specific
scope, exclusions, migrations, and exceptions in that project's coding guide
and architecture decisions.

The capability and packaging claims were verified against Tapir 1.13.29, sttp
client 4.0.26, and sttp-apispec 0.11.10 in July 2026. The worked example was
compiled against the same baseline. Re-check these claims and the example when
upgrading any of those libraries.

## Contract ownership

Define each API once as shared Tapir `Endpoint` values:

```text
shared Tapir endpoints
  |-> server adapter
  |     `-> ArmeriaCatsServerInterpreter -> TapirService -> Armeria Server
  |-> JVM client adapter
  |     `-> SttpClientInterpreter -> ArmeriaCatsBackend -> HTTP
  `-> OpenAPI documentation interpreter
```

The shared contract module may own transport DTOs, API error ADTs, explicit
JSON codecs, Tapir schemas, endpoint metadata, and the complete endpoint list.
It must not contain server logic, choose `IO`, import a server or client
interpreter, or depend on Armeria.

All rules in the base guide's
[Shared HTTP API contracts](scala-coding-style.md#shared-http-api-contracts)
section apply here without restatement. This guide additionally requires
explicit or semi-automatic schema instances for public transport DTOs; do not
import `sttp.tapir.generic.auto.*` in contract modules.

## Server and client stack

Use these exact roles and names consistently:

| Role | Module | API |
| --- | --- | --- |
| Cats Effect server interpreter | `tapir-armeria-server-cats` | `ArmeriaCatsServerInterpreter` |
| sttp v4 client interpreter | `tapir-sttp-client4` | `SttpClientInterpreter` |
| JVM non-streaming client backend | `armeria-backend-cats` | `ArmeriaCatsBackend` |
| JVM streaming client backend | `armeria-backend-fs2` | `ArmeriaFs2Backend` |
| JVM streaming client interpreter | `tapir-sttp-client4` | `StreamSttpClientInterpreter` |
| Secure client interpretation | `tapir-sttp-client4` | `SttpClientInterpreter.toSecureClient` |
| In-memory server test adapter | `tapir-sttp-stub4-server` | `TapirStubInterpreter` |
| Endpoint verification | `tapir-testing` | `sttp.tapir.testing.EndpointVerifier` |
| OpenAPI generation | `tapir-openapi-docs` + `"com.softwaremill.sttp.apispec" %% "openapi-circe-yaml"` | `sttp.tapir.docs.openapi.OpenAPIDocsInterpreter` |
| OpenAPI compatibility verification | `tapir-openapi-verifier` | `sttp.tapir.docs.openapi.OpenAPIVerifier` |

Compose `TapirStubInterpreter` with
`sttp.client4.testing.BackendStub`; the Tapir interpreter alone is not an sttp
backend. Describe distinct error representations with Tapir's `oneOf` and
`oneOfVariant` outputs.

Declare `openapi-circe-yaml` directly in the consuming project; it appears only
with test scope in the `tapir-openapi-docs` POM and is not a runtime transitive
dependency.

The server composition root attaches boundary logic to shared endpoints,
creates `ArmeriaCatsServerInterpreter`, converts the resulting
`ServerEndpoint` values to a `TapirService`, and mounts that service on the
Armeria `Server`.

The JVM client composition root creates an `ArmeriaCatsBackend` as one
`Resource` for the client lifetime and passes it to `SttpClientInterpreter`.
Do not allocate a backend per request. A Scala.js or Native client may use the
backend supported by its platform, but it must interpret the same shared
endpoint definitions.

`ArmeriaCatsServerInterpreter` and the Armeria Cats Effect client backends
require `Async[F]`. Keep that capability at their composition roots and
boundary adapters. Do not widen domain algebras or programs to `Async` because
the selected transport stack needs it.

Select the Tapir and sttp client versions centrally and keep each artifact
family internally aligned. Treat `com.softwaremill.sttp.apispec`, including
`openapi-circe-yaml`, as a third centrally selected version family, and verify
its convergence with `tapir-openapi-docs`. Also verify the Armeria artifacts
pulled by the server and client integrations; do not force an Armeria version
until both integrations are known to support it.

## Worked example

Assume `CreateWidgetInput`, `CreateWidgetError`, and `WidgetView` declare
explicit Circe codecs and semi-automatic Tapir schemas beside the transport
types. The contract object gives public values explicit types and exposes one
complete registry:

```scala
import sttp.tapir.*
import sttp.tapir.json.circe.*

object WidgetApi:
  val create: PublicEndpoint[
    CreateWidgetInput,
    CreateWidgetError,
    WidgetView,
    Any,
  ] =
    endpoint.post
      .name("createWidget")
      .in("widgets")
      .in(jsonBody[CreateWidgetInput])
      .errorOut(jsonBody[CreateWidgetError])
      .out(jsonBody[WidgetView])

  val all: List[AnyEndpoint] = List(create)
```

The server and client interpret that same `WidgetApi.create` value. These
assembly functions keep resources outside the contract and make their result
types explicit:

```scala
import cats.effect.Async
import cats.effect.std.Dispatcher
import sttp.capabilities.fs2.Fs2Streams
import sttp.client4.Backend
import sttp.model.Uri
import sttp.tapir.DecodeResult
import sttp.tapir.client.sttp4.SttpClientInterpreter
import sttp.tapir.server.armeria.TapirService
import sttp.tapir.server.armeria.cats.ArmeriaCatsServerInterpreter

type WidgetClient[F[_]] =
  CreateWidgetInput => F[DecodeResult[Either[CreateWidgetError, WidgetView]]]

def widgetService[F[_]: Async](
    dispatcher: Dispatcher[F],
    createWidget: CreateWidgetInput => F[Either[CreateWidgetError, WidgetView]],
): TapirService[Fs2Streams[F], F] =
  ArmeriaCatsServerInterpreter[F](dispatcher)
    .toService(WidgetApi.create.serverLogic(createWidget))

def widgetClient[F[_]](
    baseUri: Uri,
    backend: Backend[F],
): WidgetClient[F] =
  SttpClientInterpreter()
    .toClient(WidgetApi.create, Some(baseUri), backend)
```

At the outer composition root, acquire the dispatcher, running server, and
client backend in one resource graph. In this excerpt, `startServer` builds an
Armeria `Server` that mounts `widgetService`:

```scala
import cats.effect.{Async, Resource}
import cats.effect.std.Dispatcher
import com.linecorp.armeria.server.Server
import sttp.client4.Backend
import sttp.client4.armeria.cats.ArmeriaCatsBackend

final case class WidgetRuntime[F[_]](
    server: Server,
    backend: Backend[F],
)

def widgetRuntime[F[_]: Async](
    startServer: Dispatcher[F] => Resource[F, Server],
): Resource[F, WidgetRuntime[F]] =
  for
    dispatcher <- Dispatcher.parallel[F]
    server     <- startServer(dispatcher)
    backend    <- ArmeriaCatsBackend.resource[F]()
  yield WidgetRuntime(server, backend)
```

## Streaming and WebSocket limits

`ArmeriaCatsBackend` does not support non-blocking streaming. For a streaming
JVM client, use `ArmeriaFs2Backend` with
`sttp.tapir.client.sttp4.stream.StreamSttpClientInterpreter`. Prefer
`ArmeriaFs2Backend.resource[F]` at the composition root. Non-`Resource`
construction also requires a `Dispatcher[F]`, whose lifetime the composition
root must own.

The fixed stack does not support Tapir WebSocket endpoints:

- the Armeria Cats Effect server interpreter accepts the `Fs2Streams[F]`
  capability, not `Fs2Streams[F] & WebSockets`; and
- the sttp Armeria Cats and FS2 backends do not implement WebSocket support.

Armeria itself has a native `WebSocketService`, and Tapir has
`WebSocketSttpClientInterpreter`, but combining those APIs would no longer give
the fixed stack one shared server/client contract. A project that requires
WebSockets must therefore select a different end-to-end stack or record a
narrowly documented exception before implementation.

## Unix domain sockets

Armeria 1.24 or newer can serve and send HTTP over Unix domain sockets. Apply
this operating checklist only to a JVM-to-JVM local HTTP API approved by the
adopting project's architecture:

- Isolate Armeria's unstable `DomainSocketAddress` and native channel types
  behind a small runtime interpreter.
- Construct a `WebClient` from the socket address's URI authority and pass it to
  `ArmeriaCatsBackend.usingClient`.
- Own the corresponding `ClientFactory` in the same composition-root resource
  graph as the backend.
- Verify the macOS and Linux native transports in packaged and test
  environments against the pinned dependency versions.
- Preserve socket-directory ownership, permissions, symlink defenses, path
  length checks, and launch authentication required by the threat model.
- Keep an explicitly tested Windows loopback fallback.
- Remove stale socket files on startup and shutdown without hiding a primary
  failure.

Do not use this checklist to replace an existing non-HTTP IPC transport without
a separately versioned protocol and architecture decision.

## Verification

Verify a structured HTTP contract at four distinct levels:

1. Run `EndpointVerifier` over the complete endpoint list.
2. Use `sttp.tapir.server.stub4.TapirStubInterpreter` to send calls produced by
   the client interpreter through the corresponding `ServerEndpoint`, covering
   success, each expected error variant, and representative decode failures.
3. In focused wire-contract tests, deliberately bypass the client interpreter
   and send hand-written sttp requests through the appropriate Armeria backend
   to the real service. This test-only exception pins server options, status,
   headers, body encoding, interceptors, connection behavior, streaming, Unix
   sockets, and lifecycle independently of the shared endpoint definition.
4. For a published or independently deployed API, compare the generated
   OpenAPI document with the released compatibility baseline using
   `OpenAPIVerifier` or an equivalent compatibility gate.

A test derived entirely from one endpoint value proves interpreter agreement,
but it cannot prove that a simultaneous server/client contract change was
intentional.

## References

- [Tapir Armeria server interpreter](https://tapir.softwaremill.com/en/latest/server/armeria.html)
- [Tapir sttp v4 client interpreter](https://tapir.softwaremill.com/en/latest/client/sttp4.html)
- [Tapir OpenAPI generation](https://tapir.softwaremill.com/en/latest/docs/openapi.html)
- [Tapir schema derivation](https://tapir.softwaremill.com/en/latest/endpoint/schemas.html)
- [Tapir testing and verification](https://tapir.softwaremill.com/en/latest/testing.html)
- [sttp Cats Effect and Armeria backends](https://sttp.softwaremill.com/en/stable/backends/catseffect.html)
- [Armeria 1.24 Unix domain socket support](https://armeria.dev/release-notes/1.24.0/)
