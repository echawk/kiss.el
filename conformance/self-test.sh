#!/bin/sh
# Verify the harness itself with two deliberately identical mock KISS CLIs.
set -eu
base=$(mktemp -d "${TMPDIR:-/tmp}/kiss-conformance-self.XXXXXX")
trap 'rm -rf "$base"' EXIT HUP INT TERM
for name in reference candidate; do
    cat > "$base/$name" <<'EOF'
#!/bin/sh
case "$1" in
  version) printf '6.2.0\n' ;;
  list) printf 'demo 1-1\n' ;;
  *) exit 2 ;;
esac
EOF
    chmod 755 "$base/$name"
done
exec "$(dirname "$0")/run.sh" "$base/reference" "$base/candidate"
