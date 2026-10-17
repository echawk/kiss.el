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

The current local run against `~/.kiss/kiss` passes all 39 harness cases.
This includes removal refusal/status records, build lifecycle hooks, install and
remove hook exit behavior, alternatives, and preferred-provider output.

| Area | Needed change | Evidence |
|---|---|---|
| Command results | Give every public command an explicit CLI formatter and status contract; action functions currently often return nil on success. | Commands not yet exercised by the harness still need explicit records. |
| Diagnostics | Keep concise stderr diagnostics for every action failure unless debug mode is enabled. | Extend the current CLI error path to every public command. |
| Argument validation | Reject unsupported arity/options with status 1 and usage text. | Aliases, `help`, defaults, and extensions remain unimplemented. |
| Noninteractive policy | Read and validate `KISS_CHOICE`, `KISS_FORCE`, stripping, and prompt variables from the process environment. | Conflict and prompt cases. |
| Root safety | Audit all path construction and privilege adapters after `KISS_ROOT` is introduced. | Install/remove must never escape the redirected root. |
| Output compatibility | Match exact upstream records for owns, checksum, update, and upgrade. | Alternatives and preferred are covered; add differential cases for the remaining commands. |
| Hook protocol | Extend the current install/remove/build coverage to source and update event families. | Current harness hook cases pass. |

## Verification

Use a known KISS executable as the reference:

```sh
./kiss-spec/conformance/run.sh ~/.kiss/kiss ./elkiss version list-installed search-shadowing
```

Do not run mutating differential cases against `elkiss` until the root-safety
row is complete.  The harness itself uses temporary roots; an implementation
which ignores `KISS_ROOT` can instead address the live system root.
