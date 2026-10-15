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

export KISS_TEST_SELECTOR="${1:-${KISS_TEST_SELECTOR:-}}"
export KISS_TEST_EXCLUDE="${KISS_TEST_EXCLUDE:-}"

exec "$EMACS" -Q --batch -L . \
  --eval "(progn (require 'package) (package-initialize) (load (expand-file-name \"test/run-ert.el\" default-directory)))"
