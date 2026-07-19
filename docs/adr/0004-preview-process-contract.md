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
    "args": ["--server", "--batch", "doctrellisPreview"],
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

Represent process acquisition and VS Code registrations as `Resource`. On
POSIX, spawn each command as a dedicated owned process group; on Windows,
target the spawned root PID and its descendants with `taskkill /T`. Normal stop
requests graceful termination and waits up to three seconds. POSIX polls the
owned group; Windows requires successful tree termination plus root exit and
close confirmation. It then uses a bounded exact-owner force fallback. Never
discover targets by process name or listening port. Folder removal, extension
deactivation, failed startup, restart, and explicit stop all run the same
idempotent release path. A replacement session cannot launch until release has
completed.

sbt 2 can otherwise route a default invocation through an `sbtn` thin client
and leave a detached server JVM. The recommended version 1 argument vector is
therefore `--server`, `--batch`, `doctrellisPreview`, which keeps the foreground
JVM inside the extension-owned tree. Version 1 remains compatible with other
argument vectors and arbitrary commands: the extension passes configuration
verbatim and never inserts sbt-only options.

The direct-spawn contract also applies on Windows. A configured command must
resolve to a native executable, such as an `sbt.exe` package-manager shim or an
absolute path to one. Modern Node versions reject direct `.bat`/`.cmd` spawning
without a shell, and DocTrellis does not enable a shell because doing so would
change the argument and injection-safety contract. The foreground argument
vector is unchanged.

Windows tree identity is rooted in the spawned PID and `taskkill /T`. A
non-zero graceful `taskkill` result always triggers the `/F` pass and is never
treated as successful merely because the root disappeared. If a third-party
launcher independently detaches descendants and exits before that fallback,
Windows no longer exposes an exact tree handle through this API; DocTrellis
reports cleanup failure and retains the session rather than guessing by name or
port. Using the documented foreground native shim avoids that residual
root-disappears-first limitation.

No command runs on activation. Start/restart/open require a trusted workspace;
stop and log display remain available so an existing process can be inspected
or cleaned up.

## Consequences

- Configuration errors are reported before process launch.
- Shell metacharacters in arguments are data rather than executable syntax.
- Readiness reflects the service response rather than an arbitrary delay.
- Multi-root workspaces have isolated sessions and an explicit folder choice.
- Version 1 cannot express environment variables, shell pipelines, readiness
  headers/bodies, or alternative shutdown signals. Those require a versioned
  contract rather than ad hoc optional behavior.
- ADR 0005 adds version 2's IPC-announced endpoint and dynamic HTTP port while
  retaining every version 1 process and fixed-URL behavior described here.
