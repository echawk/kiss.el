# `elkiss` CLI conformance plan

`elkiss` is the executable boundary used by the differential harness.  The
library can be exercised directly by ERT while still failing every CLI case if
this boundary does not implement the KISS process contract.  This document
tracks that boundary separately from internal library behaviour.

## Implemented in the current change

- Read `KISS_ROOT` at process startup and derive the installed and choice
  databases beneath it.
- Prefer `XDG_CACHE_HOME/kiss/` for temporary builds, sources, binary archives,
  and logs; retain `HOME/.cache/kiss/` as the portable fallback.
- Emit CLI result data to stdout, never through Emacs `message`.
- Return status 1 for an unknown command, a Lisp error, or an empty `list` /
  `search` result; return status 0 for successful commands.
- Format `version`, `list`, and `search` as line-oriented KISS CLI output.

## Remaining CLI work

The current local run against `~/.kiss/kiss` passes 31 of 39 harness cases.
The remaining named cases are `remove-missing`, `remove-blocked-dependent`,
`remove-active-alternative`, `build-recipe`, `install-hooks`,
`install-hook-failure`, `alternatives-list`, and `preferred-active`.

| Area | Needed change | Evidence |
|---|---|---|
| Command results | Give every public command an explicit CLI formatter and status contract; action functions currently often return nil on success. | Removal and alternatives cases still fail. |
| Diagnostics | Keep concise stderr diagnostics for every action failure unless debug mode is enabled. | Build/hook failures need an explicit CLI error path. |
| Argument validation | Reject unsupported arity/options with status 1 and usage text. | Aliases, `help`, defaults, and extensions remain unimplemented. |
| Noninteractive policy | Read and validate `KISS_CHOICE`, `KISS_FORCE`, stripping, and prompt variables from the process environment. | Conflict and prompt cases. |
| Root safety | Audit all path construction and privilege adapters after `KISS_ROOT` is introduced. | Install/remove must never escape the redirected root. |
| Output compatibility | Match exact upstream records for alternatives, preferred, owns, checksum, update, and upgrade. | `alternatives-list` and `preferred-active` currently fail. |
| Hook protocol | Verify `KISS_HOOK` discovery, arguments, working directories, event order, and nonzero propagation through the CLI. | Build and install hook cases currently fail. |

## Verification

Use a known KISS executable as the reference:

```sh
./conformance/run.sh ~/.kiss/kiss ./elkiss version list-installed search-shadowing
```

Do not run mutating differential cases against `elkiss` until the root-safety
row is complete.  The harness itself uses temporary roots; an implementation
which ignores `KISS_ROOT` can instead address the live system root.
