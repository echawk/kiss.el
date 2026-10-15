#!/bin/sh
# kiss.el test runner — runs the hermetic ERT suite in test/test-*.el.
# Usage: ./run-tests.sh [selector]
#   selector: optional ERT test-name regexp (default runs everything).
# Exit status is 0 iff every test passed.
#
# The legacy root test.el targets a live KISS installation (real
# /var/db/kiss, network clones) and is excluded by default.  Run it on a
# KISS machine with KISS_TEST_INCLUDE_LEGACY=1.
set -e
cd "$(dirname "$0")"

EMACS="${EMACS:-emacs}"

# tsort is a hard build dependency of kiss.el (kiss-package.el requires it);
# the vendored copy lives next to the sources and -L . makes it findable.
[ -f tsort.el ] || cp /tmp/kisscheck/tsort.el . 2>/dev/null || true

export KISS_TEST_SELECTOR="${1:-${KISS_TEST_SELECTOR:-}}"
export KISS_TEST_EXCLUDE="${KISS_TEST_EXCLUDE:-}"

exec "$EMACS" -Q --batch -L . \
  --eval "(load (expand-file-name \"test/run-ert.el\" default-directory))"
