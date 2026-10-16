# KISS differential conformance harness

`run.sh` compares two KISS-compatible command-line implementations against
fresh, independent fixtures.  It never uses the live package database, caller
cache, network, or privilege escalation.

```sh
./conformance/run.sh /tmp/kiss/kiss /path/to/other-kiss
./conformance/run.sh /tmp/kiss/kiss /path/to/other-kiss version
```

Each implementation must be an executable path that honors the standard KISS
CLI and these environment variables: `KISS_ROOT`, `KISS_PATH`,
`XDG_CACHE_HOME`, `HOME`, and `KISS_PROMPT=0`.  It compares command exit
status, specification-defined stdout, and a normalized snapshot of the
resulting `KISS_ROOT`.  Stderr is retained for diagnostics but is not compared,
because progress wording is intentionally non-normative.

Built-in cases are deliberately small and portable:

- `version`: command output and status;
- `list-installed`: an installed recipe yields the specified list record.

To add a case, extend `setup_case` in `run.sh` with only temporary fixture
files, select its argument vector, and rely on the existing comparison loop.
Add root snapshots for mutations and exact stdout only when the KISS spec makes
that output machine-consumable.  Avoid network URLs, host paths, interactive
prompts, and implementation-specific extensions.

This is opt-in: `./run-tests.sh` remains a standalone hermetic test suite and
does not require the reference shell script.
