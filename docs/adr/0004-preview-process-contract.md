# ADR 0004: Preview process contract

- Status: Accepted
- Date: 2026-07-19

## Context

A workspace configuration can cause an arbitrary local executable to run. The
extension therefore needs an explicit, narrow contract, early parsing, clear
trust behavior, and deterministic cleanup. It must also distinguish “the
process exists” from “the HTTP preview is ready.”

## Decision

Use `.doctrellis.json` schema version 1 as the complete process contract:

```json
{
  "version": 1,
  "preview": {
    "command": "sbt",
    "args": ["doctrellisPreview"],
    "workingDirectory": ".",
    "url": "http://127.0.0.1:4242",
    "startupTimeoutMs": 90000
  }
}
```

The parser rejects missing fields, additional fields, wrong types, unsupported
versions, empty/NUL-containing executable and path values, non-HTTP(S) URLs,
and timeouts outside 1–600000 ms. Resolve the workspace and candidate working
directory with filesystem real paths, then reject candidates whose relative
path escapes the workspace. This also covers symlink escapes.

Pass `command` and `args` separately to Node `spawn` with `shell = false`. Stream
stdout and stderr to the `DocTrellis` Output Channel.

Model one session per workspace folder. Starting an existing session reuses it;
restart performs stop followed by start. A supervised Cats Effect fiber races:

- polling the configured URL until a 200–399 response arrives; and
- child-process exit.

Each HTTP attempt has a short request timeout and the overall loop uses
`startupTimeoutMs`. An early process exit or overall timeout fails startup with
a message that points users to the output channel.

Represent process acquisition and VS Code registrations as `Resource`. Normal
stop first sends `SIGTERM`, waits up to three seconds, then sends `SIGKILL` and
waits once more. Folder removal, extension deactivation, failed startup, and
explicit stop all run the same idempotent release path.

No command runs on activation. Start/restart/open require a trusted workspace;
stop and log display remain available so an existing process can be inspected
or cleaned up.

## Consequences

- Configuration errors are reported before process launch.
- Shell metacharacters in arguments are data rather than executable syntax.
- Readiness reflects the service response rather than an arbitrary delay.
- Multi-root workspaces have isolated sessions and an explicit folder choice.
- The initial schema cannot express environment variables, shell pipelines,
  readiness headers/bodies, or alternative shutdown signals. Those require a
  future versioned contract rather than ad hoc optional behavior.
