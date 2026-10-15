# Project layout

`kiss.el` is an Emacs Lisp implementation of the KISS package manager. The
module boundaries separate data parsing and dependency logic from filesystem
and process operations, making a later Common Lisp or Scheme port practical.

```text
.
├── kiss.el                 public package-manager commands and orchestration
├── kiss-env.el             configuration, environment, and command mappings
├── kiss-file.el            filesystem, archive, checksum, and ELF helpers
├── kiss-os.el              process/user identity boundary
├── kiss-source.el          source-record model, parsing, cache, acquisition
├── kiss-package.el         package-record model and dependency algorithms
├── kiss-manifest.el        manifest generation, validation, serialization
├── kiss-install.el         binary-package installation
├── kiss-build.el           build environment and package construction
├── kiss-download.el        download command orchestration
├── kiss-search.el          repository/database lookup
├── kiss-update.el          repository update orchestration
├── kiss-preferred.el       alternatives' selected-owner lookup
├── kiss-hook.el            global and package hook execution
├── tsort (MELPA dependency) external topological-sort implementation
├── elkiss                  thin command-line wrapper around public commands
├── kiss-test-fixture.el    isolated KISS root/cache test fixture
├── test/
│   ├── run-ert.el          hermetic ERT test entry point
│   ├── test-kiss-core.el   parser, manifest, graph, lookup invariants
│   └── test-kiss-file.el   file, archive, checksum, and manifest invariants
├── run-tests.sh            default test runner
├── test.el                 legacy live-system tests (opt-in only)
├── README.org              user documentation
└── LICENSE                 MIT license
```

## Effects and test policy

Pure helpers carry Emacs declarations where that is sound: path transforms,
permission-triad conversion, manifest serialization, version sanitization,
binary-file classification, and library-name classification. Source-record
parse/serialization are explicitly marked side-effect-free instead: the source
object is mutable. These are the best first candidates for a functional port.

Functions that read the filesystem, run a process, mutate package state, or
consult dynamic configuration are deliberately *not* declared pure. Their
effects are kept behind the `file`, `os`, `source`, `build`, `install`, `hook`,
and `update` modules instead of being mislabeled for compiler optimization.

`./run-tests.sh` loads only `test/test-*.el`. Those tests create temporary
roots and caches; they never use the host `/var/db/kiss`. `test.el` is a
legacy integration suite and is loaded only with `KISS_TEST_INCLUDE_LEGACY=1`.

## KISS conformance baseline

The implementation and tests are checked against the KISS 6.2.0 port/package
format: absolute root-relative, reverse-sorted manifest records; 66-character
`b3sum -l 33` checksums; two-field versions; `SOURCE [DESTINATION]` records;
comment-free dependency/source payloads; and dependency-before-dependent build
ordering. The implementation also supports Mercurial and Fossil sources as a
documented extension; portable recipes should use the normative Git/URL/local
forms.

## Portability audit

The package-format layer now uses plain property lists for package, source,
and build-environment records; they correspond directly to Common Lisp plists
and can be represented as Scheme alists or SRFI records. It no longer uses
EIEIO classes, slot accessors, or object mutation. The dependency sorter is
the separately installed `tsort` MELPA dependency, not a vendored Emacs
library; a Scheme/CL port can use an equivalent Kahn sort implementation.

The remaining Emacs-specific features are deliberately limited to adapters:
`defcustom`/`interactive` UI configuration, buffers and file attributes,
subprocess execution, and dynamic environment binding. These need target
runtime adapters but do not define KISS data semantics. `pcase`, `seq`, `rx`,
and `thread-last` are syntactic/list-processing conveniences: `pcase` maps to
`match`/Trivia as planned, while the sequence operations map to SRFI-1 and
the threading/regex forms are straightforward nested calls and regular
expressions. The prior speculative binary-file fast path, which interned
arbitrary strings, was replaced by the verified pure classifier.
