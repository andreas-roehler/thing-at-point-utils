;;; ar-thingatpt-interactive-tests.el --- tests succeeding when called interactively  -*- lexical-binding: t; -*-

;; Copyright (C) 2018-2025  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@online.de>
;; Keywords: convenience

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

(ert-deftest ar-delimited-test-V5mQXw ()
  (ar-test-point-min
      "srcdir=@srcdir@
# MinGW CPPFLAGS may use this.
abs_top_srcdir=@abs_top_srcdir@
"
    'sh-mode
    nil
    (goto-char (point-min))
    (search-forward "@s")
    (sit-for 0.1)
    (should (string=  (ar-delimited-atpt) "@srcdir@"))))

(ert-deftest ar-emacs-lisp-sexp-test-6WqoA8 ()
  (ar-test
      "((asdf)\")\")"
    'emacs-lisp-mode
    ar-debug-p
    (goto-char (point-max))
    (ar-backward-sexp)
    (should (eq (char-after) ?\())))

(provide 'ar-thingatpt-interactive-tests)
;;; ar-thingatpt-interactive-tests.el ends here
