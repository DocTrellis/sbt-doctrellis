# ADR 0005: Workspace IPC and dynamically bound preview ports

- Status: Accepted
- Date: 2026-07-19
- Extends: ADR 0004
- Extended by: ADR 0006
- Related: ADR 0009

## Context

Schema version 1 makes the plugin and extension independently know a fixed
HTTP URL. Polling that URL cannot prove which process owns it, prevents two
projects from safely sharing a default port, and encourages racy “find a free
port, close it, then bind it later” workarounds.

## Decision

Schema version 2 separates process configuration from the endpoint it will
announce:

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

The plugin binds the HTTP server directly to `127.0.0.1:0`, reads the positive
port from the bound server address, and only then announces the URL. Direct CLI
use retains `doctrellisPort := 4242`; port zero is an additional opt-in value.
The domain represents these modes as `PreviewEndpoint.Fixed(url)` and
`PreviewEndpoint.Announced`. A session stores the announced URL, and the HTTP
probe, Webview, restart, and external-browser path all use that stored value.

The Preview JVM owns a project-keyed control server. macOS and Linux use a Unix
domain socket. Windows uses a JDK-portable, loopback-only TCP control socket
with the same token-authenticated semantics because the JDK has no portable
named-pipe server API. Platform selection is hidden behind the workspace
channel adapter. The extension and JVM run on the same Remote SSH or Dev
Container host; the IPC address is never passed through VS Code URI forwarding.

The extension canonicalizes the workspace and uses the first 24 hexadecimal
characters of its SHA-256 digest. Endpoints live in a user-specific temporary
directory, outside the project, with directory mode `0700` and Unix endpoint
files mode `0600` when POSIX permissions are available. Both runtimes reject a
symlinked directory or one not owned by the current UID. Unix paths are capped
at 100 UTF-8 bytes, and each server uses a session-unique socket filename. A
stable per-workspace file lock serializes stale cleanup, metadata creation, and
socket bind across competing JVMs; the `CREATE_NEW` metadata record identifies
the current session. An existing endpoint is removed while holding that lock
only after an authenticated protocol handshake fails and its recorded owner
PID is confirmed dead. Malformed metadata, a live PID, or a valid handshake
protects the endpoint. The empty lock file is persistent coordination state;
metadata and session sockets are removed on shutdown.

Each extension launch generates a launch ID, and each server generates a
256-bit random token and session ID. The launch ID crosses the process boundary
in the environment and is repeated in endpoint metadata and the `ready`
response. The token is stored only in the current-user endpoint metadata, sent
on control requests, compared in constant time by the plugin, and never logged.
A second VS Code
window can authenticate and reuse the existing ready URL as a read-only lease.
Reuse is accepted only after both the correlated `ready` response and a
correlated `ping`/`pong` exchange, so a recycled Windows loopback port cannot
impersonate a live preview. When metadata identifies a live owner, reuse
retries within the configured startup timeout; a missing or dead owner proceeds
directly to a new launch and stale cleanup.
The spawning window accepts an owned connection only when all three launch IDs
match, so it remains the sole lifecycle owner; releasing an observer does not
stop another window's process. If two windows race before either server has
published `ready`, the operating-system file lock chooses one owner. The loser
cannot adopt or stop the winner and fails clearly; retrying Start after
publication reuses the winner.

The token is a local bearer credential, not mutual authentication. On Windows,
the initial `hello` can disclose a stale session's token to a process that has
recycled its loopback port. That token cannot authenticate a fresh session, is
never logged, and endpoint metadata remains restricted to the current user.

## Protocol

`doctrellis-preview-v1` is newline-delimited strict UTF-8 JSON. Readers
accumulate arbitrary transport chunks, including a UTF-8 code point split
across chunks, accept LF or CRLF, and process multiple frames per chunk. A frame
is at most 8192 bytes. Invalid UTF-8, empty, malformed, oversized,
unknown-version, and unknown-type messages are errors.

Every request contains `version`, `type`, `token`, `clientId`, and `requestId`:

- client to plugin: `hello`, `ping`, `stop`;
- plugin to client: `ready` (`url`, `pid`, `sessionId`, `launchId`), `pong`,
  `stopping`, `stopped`, and `error` (`requestId`, `code`, `message`).

The client accepts `ready` only for its hello request, metadata PID, session ID,
and launch ID. A second ready frame is rejected whether its URL is duplicate or
conflicting. `pong`, `stopping`, and `stopped` must correlate to their
outstanding request, and a duplicate or late acknowledgement is an error.
An `error` has non-empty `code` and `message` fields. Request-level errors echo
the request ID. Frame- and message-level errors use an empty request ID because
a valid correlation may not be available; the client accepts an error request
ID only when it is empty or matches its hello, outstanding ping, or stop.
Stop first waits for `stopping`/`stopped`; disconnect, timeout, or an error
falls back to the exact owned process-tree termination from ADR 0004. The single
startup timeout covers both the IPC announcement and the HTTP probe, and both
phases race the owned process exit.

Announced IPC previews require `doctrellisHost := "127.0.0.1"`; the plugin
rejects other hosts before binding. Direct fixed-port CLI previews retain the
configurable host setting.

## Compatibility

| Configuration | Plugin | Behavior |
| --- | --- | --- |
| version 1 fixed URL | old or new | Existing URL polling and verbatim command arguments |
| version 2 announced endpoint | new | Dynamic bind, IPC ready URL, authenticated stop |
| version 2 announced endpoint | old | Unsupported: no control channel, so startup times out or the process exits with a clear failure |
| direct CLI | new | Fixed port 4242 unless the user explicitly sets port 0 |

Version-specific fields are exclusive: version 1 requires `url` and rejects
`endpoint`; version 2 requires the exact IPC endpoint declaration and rejects
`url`. Dynamic Open Preview without a live session displays a Start-required
message instead of guessing a URL.

Restart from an observer window releases and reacquires its read-only lease; it
does not restart the process owned by another window. Only an owner-window
Restart stops and launches the preview process.

## Consequences

- Different projects can run concurrently without coordinating port numbers.
- HTTP ownership and graceful stop are tied to the same authenticated JVM.
- Endpoint files are temporary runtime state and never enter Git worktrees.
- Windows control traffic consumes an additional ephemeral loopback port, but
  remains same-user authenticated and invisible to the browser.
