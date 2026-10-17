# Hermetic test coverage matrix

This matrix tracks the KISS behaviours exercised by the default ERT suite.
Every default test creates temporary roots and caches; it must not read, modify,
or require a live KISS installation.  Optional programs are tested only when
they are available, and network operations are represented by local fixtures
or dispatch tests.

Status: **covered** means the ordinary Unix-safe path is exercised; **gated**
means an optional backend has a capability-gated test; **tracked** identifies
a real implementation boundary that cannot yet be exercised as a completed
feature.

| Area / invariant | Tests | Status |
|---|---|---|
| Manifest records are unique, root-relative, and cannot escape `KISS_ROOT` | `kiss-manifest/*`, `kiss-property/manifest-*` | covered |
| Recipe/source parser and package-record serialization preserve package meaning | `kiss-source/*`, `kiss-package/recipe-roundtrip`, parser properties | covered |
| Dependency graph is deterministic, ordered, and cycle-safe | `kiss-package/dependency-*`, `kiss-property/dependency-*`, `tsort/*` | covered |
| Local source cache, checksum validation, and extraction are self-contained | `kiss-source/local-*`, `kiss-source/extract-*` | covered |
| HTTP and VCS source transfer | parser/cache/dispatch tests; remote transfer is not contacted | gated (network intentionally excluded) |
| Archive generation/extraction preserves a staged tree | `kiss-file/archive-roundtrip` | covered |
| Build produces a valid package archive and archive installs cleanly | `kiss-build/hermetic-build-install-roundtrip` | covered |
| Sandbox/chroot construction and dynamic ELF dependencies | classifier tests; `kiss-build/dynamic-library-*` when `cc`/`ldd` are available | gated (sandbox remains capability-gated) |
| Install validates archive/manifest/dependencies before mutation | `kiss-lifecycle/install-*` | covered |
| Upgrade replaces files; removal respects runtime dependents | `kiss-lifecycle/upgrade-*`, `kiss-lifecycle/remove-*` | covered |
| Conflicts obey `KISS_CHOICE=0` without replacing a live file | `kiss-lifecycle/conflict-choice-zero-*` | covered |
| Alternatives, preferred choice, and orphan detection | `kiss-alternatives/*`, `kiss-preferred/*`, dependent tests | covered |
| Package and global hooks receive the documented contract and fail safely | `kiss-hook/*` | covered |
| Public list, owns, fork, build/install, and upgrade dispatch | `kiss-public/*`, lifecycle tests | covered |
| Cross-implementation CLI conformance | `kiss-spec/conformance/run.sh`: version, list/search, install/upgrade/remove, alternatives/preferred, hooks | covered by opt-in differential suite; full plan in `kiss-spec/conformance/CONFORMANCE_MATRIX.md` |
| Git repository discovery / update backend dispatch | `kiss-update/*`, local bare-remote pull | covered; network is never contacted |
| Mercurial and Fossil discovery/update | backend dispatch tests | gated (optional executables) |
| zstd magic/decompression | file magic tests | gated (optional compressor) |
| Privilege escalation and foreign-owner paths | adapter contract is mocked in every hermetic lifecycle test | gated (must not prompt or alter user ownership) |
| `kiss--shell-commands` adapter | `kiss-os/shell-commands-*` | covered |
| `/etc` three-way `etcsums` merge semantics | `kiss-lifecycle/upgrade-*etc*`, `remove-keeps-etc-*` | covered |

## Test design rules

- Tests use `kiss-test-with-fixture` for all package-manager state.
- Tests never use the network, the caller's `KISS_ROOT`, or the caller's
  package database.
- A test needing an optional executable uses `skip-unless`; a test needing
  elevated privileges exercises the adapter boundary with a local command.
- New implementation work should replace a **tracked** row with a hermetic
  invariant test in the same change.
