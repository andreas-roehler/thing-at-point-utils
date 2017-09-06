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
		  "ar-thing-at-point-utils-setup-tests.el"
		  "thing-at-point-utils-tests.el"
		  "ar-paired-delimit-tests.el"
		  "ar-trim-test.el"
		  "ar-unpaired-delimited-tests.el"
		  "ar-list-atpt-ert-tests.el"
		  "ar-thing-at-point-utils-forward-tests.el"
		  "translate-paired-delimiters-test.el"
		  "ar-thing-at-point-utils-nodelim-classes-test.el"
		  "ar-tatpt-utils-delimited-tests.el"
		  ;; "thingatpt-transform-delimited-test.el"
		  "ar-list-atpt-brace-ert-tests.el"
		  "ar-paired-delimited-tests.el"
		  ;; "ar-peel-tests.el"
		  "delimited2delimited-tests.el"
		  "ar-separate-tests.el"
		  "ar-list-atpt-parens-ert-tests.el"
		  "ar-bracketlist-atpt-ert-tests.el"
		  "ar-thing-atpt-other-test.el"
		  )))
      (load (expand-file-name "beg-end" source-directory))
      (load (expand-file-name "ar-subr" source-directory))
      (load (expand-file-name "thingatpt-utils-map.el" source-directory))
      (load (expand-file-name "thingatpt-utils-core" source-directory))
      (load (expand-file-name "thing-at-point-utils" source-directory))
      (load (expand-file-name "thingatpt-highlight.el" source-directory))
      (load (expand-file-name "thingatpt-transform-delimited.el" source-directory))
      (load (expand-file-name "thing-unpaired-delimited-list-in-rest-list.el" source-directory))
      (load (expand-file-name "thing-classes-in-rest-list.el" source-directory))
      (load (expand-file-name "thing-delimited-list-in-counts-list.el" source-directory))
      (load (expand-file-name "thing-unpaired-delimited-list-in-delimited-list.el" source-directory))
      (load (expand-file-name "thing-classes-in-delimited-list.el" source-directory))
      (load (expand-file-name "translate-paired-delimiters.el" source-directory))
      (load (expand-file-name "thing-at-point-peel.el" source-directory))
      (load (expand-file-name "thingatpt-count-ratio.el" source-directory))
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
