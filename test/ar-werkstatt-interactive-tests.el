
;;; ar-werkstatt-interactive-tests.el --- Werkstatt interactive tests

;; Copyright (C) 2014  Andreas Roehler

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


(ert-deftest in-doublequoted-ignore-escaped-test ()
  (ar-test-with-temp-buffer
      "\\\" \"asdf\""
      (emacs-lisp-mode)
    (forward-char -2)
    (let ((erg (ar-doublequoted-atpt)))
      (should (string= "\"asdf\"" erg)))))

(ert-deftest ar-list-up-atpt-test ()
  (py-test-with-temp-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-chars-forward "^(")
    (up-list)
    (should (eq 10 (char-after)))))


(ert-deftest ar-delimited-xml-test-X3opvb ()
  (ar-test
      "<rdg wit=\"a2\">Foo bar baz<milestone unit=\"stanza\"/></rdg>"
    'sgml-mode
    ar-debug-p
    (goto-char (point-max))
    (search-backward "Foo")
    (should
     (string=  (ar-delimited-atpt) ">Foo bar baz<"))))

;; ar-werkstatt-interactive-tests.el ends here
(provide 'ar-werkstatt-interactive-tests)
