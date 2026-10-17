#!/bin/sh
# Differential conformance tests for elkiss: runs the kiss-spec submodule's
# core and extended suites against the reference kiss 6.2.0.
#
# Usage: ./run-conformance-tests.sh [CASE ...]
#   With no CASE, every core and extended case runs.
#
# Environment:
#   KISS_REFERENCE  reference kiss executable (default: fetch kiss 6.2.0 into
#                   the cache below and verify its SHA-256)
#   KISS_CANDIDATE  implementation under test (default: ./elkiss)
#   KISS_CONFORMANCE_CACHE
#                   where the fetched reference is kept
#                   (default: ${XDG_CACHE_HOME:-$HOME/.cache}/kiss.el-conformance)
#
# Exit status is 0 iff the harness self-test and every selected case passed;
# with no CASE, both suites run even when the first has failures.
set -eu
cd "$(dirname "$0")"

kiss_url=https://codeberg.org/kiss-community/kiss/raw/tag/6.2.0/kiss
kiss_sha256=9874e2090290372156f5840c6b7b52d2101e3dae29e22b042831d1c6163633e4

suite=kiss-spec/conformance
if ! [ -x "$suite/run.sh" ]; then
    git submodule update --init kiss-spec
fi

sha256() {
    if command -v sha256sum >/dev/null 2>&1; then
        sha256sum "$1" | cut -d' ' -f1
    else
        shasum -a 256 "$1" | cut -d' ' -f1
    fi
}

if [ -n "${KISS_REFERENCE-}" ]; then
    reference=$KISS_REFERENCE
else
    cache=${KISS_CONFORMANCE_CACHE:-${XDG_CACHE_HOME:-$HOME/.cache}/kiss.el-conformance}
    mkdir -p "$cache/bin"
    if ! [ -f "$cache/kiss" ] || [ "$(sha256 "$cache/kiss")" != "$kiss_sha256" ]; then
        curl -fsSL -o "$cache/kiss.tmp" "$kiss_url"
        if [ "$(sha256 "$cache/kiss.tmp")" != "$kiss_sha256" ]; then
            rm -f "$cache/kiss.tmp"
            echo "reference kiss failed SHA-256 verification" >&2
            exit 1
        fi
        chmod 755 "$cache/kiss.tmp"
        mv "$cache/kiss.tmp" "$cache/kiss"
    fi
    # The reference expects GNU/busybox `wc`, which prints counts unpadded;
    # BSD `wc` pads them, which breaks the reference's removal check.
    printf '#!/bin/sh\n/usr/bin/wc "$@" | sed "s/^ *//"\n' > "$cache/bin/wc"
    printf '#!/bin/sh\nPATH="%s/bin:$PATH" exec "%s/kiss" "$@"\n' \
        "$cache" "$cache" > "$cache/kiss-ref"
    chmod 755 "$cache/bin/wc" "$cache/kiss-ref"
    reference=$cache/kiss-ref
fi

candidate=${KISS_CANDIDATE:-$PWD/elkiss}

# Collation affects glob and sort order; compare under the C locale.
export LC_ALL=C

"$suite/self-test.sh" >/dev/null

if [ "$#" -gt 0 ]; then
    exec "$suite/run.sh" "$reference" "$candidate" "$@"
fi

# run.sh stops after the core suite when it fails; run the core and extended
# inventories separately so every case is always reported.
cases() {
    sed -n "s/^$1_cases='\(.*\)'\$/\1/p" "$suite/run.sh"
}
core=$(cases core)
extended=$(cases extended)
if [ -z "$core" ] || [ -z "$extended" ]; then
    echo "cannot read case inventories from $suite/run.sh" >&2
    exit 2
fi

status=0
set -f
# shellcheck disable=2086
"$suite/run.sh" "$reference" "$candidate" $core || status=1
# shellcheck disable=2086
"$suite/run.sh" "$reference" "$candidate" $extended || status=1
exit "$status"
