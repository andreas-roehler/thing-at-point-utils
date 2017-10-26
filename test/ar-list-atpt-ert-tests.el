;;; ar-list-atpt-ert-tests.el --- ar-list-atpt-ert-tests

;; Copyright (C) 2015-2017  Andreas Röhler
;; Author: Andreas Röhler <andreas.roehler@online.de>
;; Keywords: languages, convenience

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

;;; Code:

(ert-deftest ar-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (should (eq 10 (length (ar-list-atpt))))))

(ert-deftest ar-list-bounds-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (should (and (eq 7 (caar (ar-bounds-of-list-atpt)))(eq 17 (cdr (cadr (ar-bounds-of-list-atpt))))))))

(ert-deftest ar-list-beginning-position-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (should (eq 7 (ar-list-beginning-position-atpt)))))

(ert-deftest ar-list-end-position-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (should (eq 17 (ar-list-end-position-atpt)))))

(ert-deftest ar-list-beginning-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-list-beginning-atpt)
    (should (eq 7 (point)))))

(ert-deftest ar-list-end-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-list-end-atpt)
    (should (eq 16 (point)))))

(ert-deftest ar-list-in-p-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (should (ar-list-in-p-atpt))))

(ert-deftest ar-length-of-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (should (eq 10 (ar-length-of-list-atpt)))
    (goto-char 1)
    (should (eq 17 (ar-length-of-list-atpt)))))

(ert-deftest ar-copy-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (should (eq 10 (length (ar-copy-list-atpt))))))

(ert-deftest ar-delete-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-delete-list-atpt)
    (should (eq (ar-length-of-list-atpt) 7))))

(ert-deftest ar-doublequote-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-doublequote-list-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-slash-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (should (eq 10 (length (ar-list-atpt))))))

(ert-deftest ar-doublebackslashparen-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-doublebackslashparen-list-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-doubleslash-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-doubleslash-list-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-doublebackslashparen-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-doublebackslashparen-list-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-backslashparen-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-backslashparen-list-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-dollar-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-dollar-list-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-equalize-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-equalize-list-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-greater-list-angle-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-greaterangle-list-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-lesserangle-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-lesserangle-list-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-backslash-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-backslash-list-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-backtick-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-backtick-list-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-brace-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-brace-list-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-bracket-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-bracket-list-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-hyphen-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-hyphen-list-atpt)
    (should (eq (char-before) ?\-))))

(ert-deftest ar-mark-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-mark-list-atpt)
    (should (eq 7 (region-beginning)))
    (should (eq 17 (region-end)))))

(ert-deftest ar-kill-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-kill-list-atpt)
    (should (eq (ar-length-of-list-atpt) 7))))

(ert-deftest ar-parentize-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-parentize-list-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-separate-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-separate-list-atpt)
    (should (looking-back "^ +"))
    (end-of-line)
    (should (eq (char-after) 10))))

(ert-deftest ar-singlequote-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-singlequote-list-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-triplequotesq-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-triplequotesq-list-atpt)
    (should (looking-back "'''"))))

(ert-deftest ar-triplequotedq-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-triplequotedq-list-atpt)
    (should (looking-back "\"\"\""))))

(ert-deftest ar-trim-list-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-trim-list-atpt)
    (should (eq (char-after) ?c))))

(ert-deftest ar-trim-list-left-atpt-test ()
  (ar-test-with-python-buffer-point-min
      "(list (cons 1 2))\n"
    (forward-char 1)
    (skip-syntax-forward "^(")
    (ar-trim-list-left-atpt)
    (should (eq (char-after) ?c))))


(provide 'ar-list-atpt-ert-tests)

