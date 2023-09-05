;;; ar-list-atpt-parens-tests.el --- ar-list-atpt-parens-tests

;; Copyright (C) 2015-2022  Andreas Röhler
;; Author: Andreas Roehler <andreas.roehler@online.de>
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

;; This file is generated by function from python-mode-utils.el - see in
;; directory devel. Edits here might not be persistent.

;;; Code:

(ert-deftest ar-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (should (eq 10 (length (ar-list-atpt))))))

(ert-deftest ar-list-bounds-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (should (and (eq 7 (car (ar-bounds-of-list-atpt)))(eq 17 (cdr (ar-bounds-of-list-atpt)))))))

(ert-deftest ar-list-beginning-position-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (should (eq 7 (ar-list-beginning-position-atpt)))))

(ert-deftest ar-list-end-position-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (should (eq 17 (ar-list-end-position-atpt)))))

(ert-deftest ar-list-beginning-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-list-beginning-atpt)
    (should (eq 7 (point)))))

(ert-deftest ar-list-end-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-list-end-atpt)
    (should (eq (char-after) ?\)))))

(ert-deftest ar-in-list-p-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (should (ar-in-list-p-atpt))))

(ert-deftest ar-length-of-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (should (eq 10 (ar-length-of-list-atpt)))
    (goto-char 1)
    (should (eq 17 (ar-length-of-list-atpt)))))

(ert-deftest ar-list-copy-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (should (eq 10 (length (ar-copy-list-atpt))))))

(ert-deftest ar-delete-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-delete-list-atpt)
    (should (eq (ar-length-of-list-atpt) 7))))

(ert-deftest ar-doublequote-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-doublequote-list-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-list-slash-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (should (eq 10 (length (ar-list-atpt))))))

(ert-deftest ar-doublebackslash-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-doublebackslash-list-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-doubleslash-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-doubleslash-list-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-doublebackslash-parentized-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-doublebackslash-parentized-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-dollar-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-dollar-list-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-equalize-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-equalize-list-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-greaterangle-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-greaterangle-list-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-lesserangle-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-lesserangle-list-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-backslash-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-backslash-list-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-backtick-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-backtick-list-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-brace-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-brace-list-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-bracket-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-bracket-list-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-hyphen-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-hyphen-list-atpt)
    (should (eq (char-before) ?\-))))

(ert-deftest ar-mark-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-mark-list-atpt)
    (should (eq 7 (region-beginning)))
    (should (eq 17 (region-end)))))

(ert-deftest ar-kill-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-kill-list-atpt)
    (should (eq (ar-length-of-list-atpt) 7))))

(ert-deftest ar-parentize-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-parentize-list-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-separate-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
    "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-separate-list-atpt)
    (skip-chars-backward " \t\r\n\f")
    (should (eq (char-before) ?\)))
    (back-to-indentation)
    (forward-char 1)
    (should (eq (char-after) ?c))))


(ert-deftest ar-singlequote-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-singlequote-list-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-triplequotesq-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-triplequotesq-list-atpt)
    (should (looking-back "'''"))))

(ert-deftest ar-triplequotedq-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-triplequotedq-list-atpt)
    (should (looking-back "\"\"\""))))

(ert-deftest ar-trim-list-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-trim-list-atpt)
    (should (eq (char-after) ?c))))

(ert-deftest ar-trim-list-left-elisp-test ()
  (ar-test-with-elisp-buffer-point-min
      "(list (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (ar-left-trim-list-atpt)
    (should (eq (char-after) ?c))))



(provide 'py-ert-execute-statement-test)
;;; ar-list-atpt-parens-tests.el ends here
