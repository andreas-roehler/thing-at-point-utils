;;; thing-at-point-utils-run.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Emacs User Group Berlin

;; Author: Emacs User Group Berlin <emacs-berlin@emacs-berlin.org>
;; Keywords:

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;;

;;; Code:

(defun thing-at-point-utils-runs-this-script-p ()
  t)

(defun thing-at-point-utils-run-tests-main ()
  "Main entry point of the test runner."
  (let* ((load-prefer-newer t)
         (current-file (if load-in-progress load-file-name (buffer-file-name)))
         (current-dir (file-name-directory current-file))
         (source-directory (locate-dominating-file current-file "Cask"))
         (pkg-rel-dir (format ".cask/%s/elpa" emacs-version)))
    (setq package-user-dir (expand-file-name pkg-rel-dir source-directory))
    (package-initialize)

    (message "Running tests on Emacs %s, built at %s"
             emacs-version (format-time-string "%F" emacs-build-time))

    (let ((debug-on-error t)
          (tests (list
		  "thing-at-point-utils-tests"
		  "ar-paired-delimit-tests"
		  "ar-trim-test"
		  "ar-unpaired-delimited-tests"
		  "ar-list-atpt-paren-ert-tests"
		  "ar-thing-at-point-utils-forward-tests"
		  "translate-paired-delimiters-test"
		  "ar-thing-at-point-utils-nodelim-classes-test"
		  "ar-thing-at-point-utils-delimited-tests"
		  "thingatpt-transform-delimited-test"
		  "ar-list-atpt-brace-ert-tests"
		  "ar-paired-delimited-tests"
		  "ar-peel-tests"
		  "delimited2delimited-tests"
		  "ar-separate-tests"
		  "ar-list-atpt-parens-ert-tests"
		  "ar-list-atpt-bracket-ert-tests"
		  "ar-thing-atpt-other-test"
		  )))
      (load (expand-file-name "beg-end" source-directory))
      (load (expand-file-name "ar-subr" source-directory))
      (load (expand-file-name "thing-at-point-utils" source-directory))

      (load (expand-file-name "ar-thing-at-point-utils-setup-tests" current-dir))

      (dolist (test-file tests)
        (load (expand-file-name test-file current-dir))))

    (let ((debug-on-error t)
          (ert-selector (pop argv)))
      (ert-run-tests-batch-and-exit (and "thing-at-point-utils-" ert-selector)))))

(when (and noninteractive (thing-at-point-utils-runs-this-script-p))
  (thing-at-point-utils-run-tests-main))

(provide 'thing-at-point-utils-run)
;;; thing-at-point-utils-run.el ends here
