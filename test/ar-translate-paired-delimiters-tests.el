;;; ar-translate-paired-delimiters-tests.el --- Translate delimiter chars

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

;; Translate delimited forms in region, like parentized into bracketed

;;; Code:

(require 'ar-translate-paired-delimiters)


(ert-deftest ar-paren2bracket-test ()
  (ar-test-with-temp-buffer
      "foo1 (&opti\\(onal beg end)"
      (ar-paren2bracket (point-min) (point))
    (should (eq ?\] (char-after)))))

(ert-deftest ar-paren2brace-test ()
  (ar-test-with-temp-buffer
      "foo1 (&option\\(al beg end)"
      (ar-paren2brace (point-min) (point))
    (should (eq ?\} (char-after)))))

(ert-deftest ar-bracket2paren-test ()
  (ar-test-with-temp-buffer
      "foo1 [&optional \\[beg end]"
      (ar-bracket2paren (point-min) (point))
    (should (eq ?\) (char-after)))))

(ert-deftest ar-bracket2brace-test ()
  (ar-test-with-temp-buffer
      "foo1 [&optional be\\[g end]"
      (ar-bracket2brace (point-min) (point))
    (should (eq ?\} (char-after)))))

(ert-deftest ar-brace2paren-test ()
  (ar-test-with-temp-buffer
      "foo1 {&optional beg \\{erg))}"
      (ar-brace2paren (point-min) (point))
    (should (eq ?\) (char-after)))))

(ert-deftest ar-brace2bracket-test ()
  (ar-test-with-temp-buffer
      "foo1 {&optional \\{beg end}"
      (ar-brace2bracket (point-min) (point))
    (should (eq ?\] (char-after)))))


(provide 'ar-translate-paired-delimiters-tests)
;;; ar-translate-paired-delimiters-tests ends here
