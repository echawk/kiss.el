;;; run-ert.el --- entry point for the kiss.el ERT suite -*- lexical-binding: t; -*-
;;
;; Loads kiss.el, the fixture, and every test/test-*.el file, then runs
;; ERT via the standard batch reporter and exits nonzero on failure.
;; Env vars:
;;   KISS_TEST_SELECTOR       regexp of test names to run (default: all)
;;   KISS_TEST_EXCLUDE        regexp of test names to skip
;;   KISS_TEST_INCLUDE_LEGACY=1 also loads the legacy root test.el
;;                            (it targets a live KISS install).

(require 'ert)
(require 'kiss)
(require 'kiss-test-fixture)

(when (getenv "KISS_TEST_INCLUDE_LEGACY")
  (when (file-exists-p (expand-file-name "test.el" default-directory))
    (load (expand-file-name "test.el" default-directory))))

(dolist (f (sort (directory-files (expand-file-name "test" default-directory)
                                  t "^test-.*\\.el$")
                 #'string<))
  (load f))

(let* ((sel (or (getenv "KISS_TEST_SELECTOR") ""))
       (excl (or (getenv "KISS_TEST_EXCLUDE") ""))
       (selector (cond
                  ((and sel (not (string= sel ""))) sel)
                  ((and excl (not (string= excl "")))
                   (list 'satisfies
                         (lambda (t)
                           (not (string-match-p excl (ert-test-name t))))))
                  (t 't)))
       (stats (ert-run-tests-batch selector)))
  ;; ERT reports the unexpected count as an integer; in Lisp, including Emacs
  ;; Lisp, zero is truthy, so test it numerically rather than with `if'.
  (kill-emacs (if (zerop (ert-stats-completed-unexpected stats)) 0 1)))
