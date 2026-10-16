#!/bin/sh
# Differential conformance harness for KISS-compatible command-line programs.
#
# Usage: conformance/run.sh REFERENCE CANDIDATE [CASE ...]
# CASE is `version' or `list-installed'; all cases run when omitted.
# Implementations are executable paths (not shell fragments).  They receive
# KISS_ROOT, KISS_PATH, XDG_CACHE_HOME, HOME, and KISS_PROMPT=0.

set -u

usage() {
    echo "usage: $0 REFERENCE CANDIDATE [CASE ...]" >&2
    exit 2
}

[ "$#" -ge 2 ] || usage
reference=$1
candidate=$2
shift 2

[ -x "$reference" ] || { echo "reference is not executable: $reference" >&2; exit 2; }
[ -x "$candidate" ] || { echo "candidate is not executable: $candidate" >&2; exit 2; }

if [ "$#" -eq 0 ]; then
    set -- version list-installed
fi

make_recipe() {
    recipe=$1
    version=$2
    mkdir -p "$recipe"
    printf '%s 1\n' "$version" > "$recipe/version"
    printf '#!/bin/sh\nexit 0\n' > "$recipe/build"
    chmod 755 "$recipe/build"
}

setup_case() {
    fixture=$1
    case_name=$2
    root=$fixture/root
    cache=$fixture/cache
    repo_a=$fixture/repo-a
    repo_b=$fixture/repo-b
    mkdir -p "$root" "$cache" "$repo_a" "$repo_b"
    case "$case_name" in
        version)
            case_args=version
            ;;
        list-installed)
            # `list' resolves only the installed database.  Put a complete
            # installed recipe in its redirected root and compare its stable
            # machine-readable output.
            make_recipe "$root/var/db/kiss/installed/demo" 1
            case_args='list demo'
            ;;
        *)
            echo "unknown conformance case: $case_name" >&2
            return 2
            ;;
    esac
    printf '%s\n' "$case_args" > "$fixture/args"
}

snapshot_tree() {
    tree=$1
    [ -d "$tree" ] || return 0
    (
        cd "$tree" || exit 1
        # Case fixtures intentionally use portable pathnames without newlines.
        # Record entry type, mode, link target, and file checksum; directory
        # timestamps and archive byte layout are deliberately not observable.
        find . -print | LC_ALL=C sort | while IFS= read -r path; do
            if [ -L "$path" ]; then
                printf 'link %s %s %s\n' "$path" "$(ls -ld "$path" | awk '{print $1}')" "$(readlink "$path")"
            elif [ -d "$path" ]; then
                printf 'dir %s %s\n' "$path" "$(ls -ld "$path" | awk '{print $1}')"
            elif [ -f "$path" ]; then
                set -- $(cksum "$path")
                printf 'file %s %s %s %s\n' "$path" "$(ls -ld "$path" | awk '{print $1}')" "$1" "$2"
            else
                printf 'other %s\n' "$path"
            fi
        done
    )
}

run_implementation() {
    implementation=$1
    fixture=$2
    args=$(cat "$fixture/args")
    # Cases use only arguments without whitespace.  This is intentional: the
    # harness operates at the executable argv boundary, not through eval.
    set -- $args
    KISS_ROOT="$fixture/root" \
    KISS_PATH="$fixture/repo-a:$fixture/repo-b" \
    XDG_CACHE_HOME="$fixture/cache" \
    HOME="$fixture/home" \
    KISS_PROMPT=0 \
    "$implementation" "$@" >"$fixture/stdout" 2>"$fixture/stderr"
    printf '%s\n' "$?" > "$fixture/status"
    snapshot_tree "$fixture/root" > "$fixture/root.snapshot"
}

temporary_base=$(mktemp -d "${TMPDIR:-/tmp}/kiss-conformance.XXXXXX") || exit 1
trap 'rm -rf "$temporary_base"' EXIT HUP INT TERM
failed=0

for case_name do
    ref_fixture=$temporary_base/reference-$case_name
    cand_fixture=$temporary_base/candidate-$case_name
    setup_case "$ref_fixture" "$case_name" || exit $?
    setup_case "$cand_fixture" "$case_name" || exit $?
    run_implementation "$reference" "$ref_fixture"
    run_implementation "$candidate" "$cand_fixture"

    if cmp -s "$ref_fixture/status" "$cand_fixture/status" &&
       cmp -s "$ref_fixture/stdout" "$cand_fixture/stdout" &&
       cmp -s "$ref_fixture/root.snapshot" "$cand_fixture/root.snapshot"; then
        printf 'ok %s\n' "$case_name"
    else
        printf 'not ok %s\n' "$case_name" >&2
        diff -u "$ref_fixture/status" "$cand_fixture/status" || true
        diff -u "$ref_fixture/stdout" "$cand_fixture/stdout" || true
        diff -u "$ref_fixture/root.snapshot" "$cand_fixture/root.snapshot" || true
        printf '%s\n' "  reference stderr: $ref_fixture/stderr" >&2
        printf '%s\n' "  candidate stderr: $cand_fixture/stderr" >&2
        failed=1
    fi
done

exit "$failed"
