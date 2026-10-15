# KISS specification conformance gaps

This is the implementation checklist for features that are presently known to
be incomplete or non-conformant.  It distinguishes a missing behaviour from a
test that is merely capability-gated.  All new tests must remain hermetic:
temporary `KISS_ROOT`/cache only, no network, no privilege prompt, and no
interaction with a caller's installed packages.

## Priority 1: filesystem correctness

- [ ] Implement the `/etc` three-way upgrade algorithm using the old package
  `etcsums`, new package `etcsums`, and the live configuration-file checksum.
  Preserve locally modified files; install changed upstream files according to
  the KISS conflict policy.
  - Affected: `kiss-install.el`, `kiss-build.el`.
  - Tests: unchanged config is replaced, locally modified config is preserved,
    removed upstream config is handled, and positional checksum streams stay
    aligned.

- [ ] Preserve modified `/etc` files when removing a package.
  - Affected: `kiss.el` (`kiss-remove`).
  - Tests: a modified configuration file survives removal while unmodified
    payload and database records are removed.

- [x] Accept explicitly supplied compressed package archives (`.tar.gz`,
  `.tar.xz`, `.tar.bz2`, and configured compression formats) in `kiss-install`.
  Detection must not require uncompressed tar magic before decompression.
  - Affected: `kiss-install.el`.
  - Tests: build/install round trips for each available compressor and a
    malformed compressed archive rejects before package-root mutation.

## Priority 2: format and build correctness

- [ ] Validate package names, `version` fields, decimal releases, dependency
  records, and source records at recipe/archive boundaries.
  - Affected: `kiss-package.el`, `kiss-source.el`, `kiss-install.el`.
  - Tests: accepted/rejected boundary cases from the port-format grammar;
    generated invalid records must never cause database mutation.

- [x] Make source-download failure status unambiguous and fatal for all VCS
  backends.  Git, Mercurial, and Fossil commands currently do not consistently
  return a boolean success result.
  - Affected: `kiss-source.el`, `kiss-download.el`.
  - Tests: local disposable Git/Hg/Fossil fixtures; failed transfer commands
    must make `kiss-download` return nil and prevent a build.

- [ ] Apply discovered dynamic library dependencies to package metadata before
  the archive is created.
  - Affected: `kiss-build.el`.
  - Tests: a controlled `kiss-owns` mapping adds a newly discovered dependency
    exactly once; system libraries and the package itself are excluded.

- [x] Export the sanitized `KISS_ROOT` value in the build-script environment.
  - Affected: `kiss-build.el`.
  - Tests: a minimal build recipe writes its observed `KISS_ROOT` into its
    staged payload without accessing the caller's root.

## Priority 3: hooks, commands, and adapters

- [x] Implement the external `pre-source`, `post-source`, and `queue-status`
  hooks with the documented argument contract and failure semantics.
  - Affected: `kiss-hook.el`, `kiss-download.el`, `kiss-build.el`.
  - Tests: executable fixture hooks record arguments; nonzero exit is fatal
    except for the existing `build-fail` exception.

- [x] Implement the `kiss-version` public command and ensure `elkiss version`
  reports the compatibility/version values instead of silently doing nothing.
  - Affected: `kiss.el`, `elkiss`.
  - Tests: invoke the CLI in batch mode and verify successful, stable output.

- [x] Implement or remove the unused `kiss--shell-commands` stub.  If kept,
  define its failure and privilege behaviour explicitly.
  - Affected: `kiss-os.el`.
  - Tests: mocked command runner verifies ordering, result aggregation, and
    stop-on-failure behaviour without running host commands.

- [ ] Give repository update backends explicit success/failure propagation and
  test local Git/Hg/Fossil repositories.  Pulls must remain disabled in the
  default hermetic suite.
  - Affected: `kiss-update.el`.
  - Tests: local repository discovery, backend command selection, hook order,
    and failed command handling.

## Capability-gated follow-up

These are not necessarily missing features, but lack complete conformance
coverage because they require optional tools or an ownership boundary:

- [ ] zstd compression/decompression and magic-byte coverage.
- [ ] Sandboxed build/chroot command execution.
- [ ] Dynamic ELF dependency discovery against a controlled toolchain.
- [ ] Foreign-owner and privilege-escalation adapter behaviour.
- [ ] Network HTTP and VCS source transfers (use local protocol fixtures if
  possible; never contact public hosts from the default suite).
