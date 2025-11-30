;;; ar-thingatpt-utils-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2025  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@easy-emacs.de>
;; Keywords: lisp

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



;; moved to ar-werkstatt-interactive-tests.el
;; as failing from shell

(ert-deftest ar-in-doublequoted-test-szpCWi ()
  (ar-test-point-min
      "\"
;;;\" \"Write 'etc. \""
    'emacs-lisp-mode
    'ar-verbose-p
    (goto-char (point-min))
    (forward-char 2)
    (let ((erg (ar-doublequoted-atpt)))
      (should (eq 6 (length erg))))))

(ert-deftest ar-in-comment-p-test-LYSuTO ()
  (ar-test-point-min
      "
\"
;;;\""
    'emacs-lisp-mode
    'ar-verbose-p
    (goto-char (point-min))
    (should (not (ar-in-comment-p-atpt)))))

(ert-deftest ar-in-comment-p-test-MSWlgv ()
  (ar-test
      "
\"
;;;\""
    'emacs-lisp-mode
    'ar-verbose-p
    (goto-char (point-max))
    (should (not (ar-in-comment-p-atpt)))))

(ert-deftest ar-in-comment-p-test-9Wt1e4 ()
  (ar-test
    "
;;; "
    'emacs-lisp-mode
    'ar-verbose-p
    (goto-char (point-max))
    (should (ar-in-comment-p-atpt))))

(ert-deftest ar-beginning-of-list-atpt-test-1EIvbZ ()
  (ar-test-point-min
      "(car (cons 1 2))"
    'emacs-lisp-mode
    'ar-verbose-p
    (forward-char 1)
    (skip-chars-forward "^(")
    (forward-char 1)
    (should (eq 6 (ar-beginning-of-list-atpt)))))

(ert-deftest ar-trim-underscored-atpt-JnI06E ()
  (ar-test-with-elisp-buffer
      "_asdf_"
    'emacs-lisp-mode
    'ar-verbose-p
    (goto-char (point-max))
    (skip-chars-backward " \t\r\n\f")
    (forward-char -2)
    (ar-trim-underscored-atpt)
    (goto-char (point-min))
    (should (eq (char-after) ?a))))

(ert-deftest ar-doublebackslashparen-alnum-atpt-2LkYLb ()
  ""
  (ar-test-point-min
      "_(asdf)_"
    'python-mode
    'ar-verbose-p
    (goto-char (point-min))
    (search-forward "a")
    (ar-doublebackslashparen-alnum-atpt)
    (should (eq (char-before) 41))
    (search-backward "a")
    (should (eq (char-before) 40))))

(ert-deftest ar-ert-sort-list-test-oz1CKn ()
  (ar-test-with-elisp-buffer
      "(list ?> ?<)"
      (goto-char (point-max))
    (backward-char)
    (ar-sort-list-atpt)
    (should (looking-back "\\?< \\?>)" (line-beginning-position)))
    ))

(provide 'ar-thingatpt-utils-tests)
;;; ar-thingatpt-utils-tests.el ends here
