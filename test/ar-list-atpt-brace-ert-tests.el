;;; ar-list-atpt-brace-tests.el --- ar-list-atpt-brace-tests

;; Copyright (C) 2015  Andreas Roehler
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

(ert-deftest ar-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (should (eq 54 (length (ar-list-atpt))))))

(ert-deftest ar-list-bounds-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (sit-for 0.1) 
    (should (and (eq 47 (caar (ar-bounds-of-list-atpt)))(eq 101 (cdr (cadr (ar-bounds-of-list-atpt))))))))

(ert-deftest ar-list-beginning-position-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (sit-for 0.1) 
    (should (eq 47 (ar-list-beginning-position-atpt)))))

(ert-deftest ar-list-end-position-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (sit-for 0.1) 
    (should (eq 101 (ar-list-end-position-atpt)))))

(ert-deftest ar-list-beginning-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-list-beginning-atpt)
    (should (eq 47 (point)))))

(ert-deftest ar-list-end-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (sit-for 0.1) 
    (ar-list-end-atpt)
    (should (eq 100 (point)))))

(ert-deftest ar-in-list-p-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (should (ar-in-list-p-atpt))))

(ert-deftest ar-length-of-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (should (eq 54 (ar-length-of-list-atpt)))
    (goto-char (point-min))
    (end-of-line)
    (should (eq 79 (ar-length-of-list-atpt)))))

(ert-deftest ar-copy-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (should (eq 54 (length (ar-copy-list-atpt))))))

(ert-deftest ar-delete-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-delete-list-atpt)
    (should (eq 25 (ar-length-of-list-atpt)))))

(ert-deftest ar-doublequote-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-doublequote-list-atpt)
    (should (eq (char-before) ?\"))))

(ert-deftest ar-slash-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-slash-list-atpt)
    (should (looking-back "}/")))) 

(ert-deftest ar-doublebackslash-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-doublebackslash-list-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-doubleslash-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-doubleslash-list-atpt)
    (should (eq (char-before) ?/))))

(ert-deftest ar-doublebackslashparen-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-doublebackslashparen-list-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-slashparen-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-slashparen-list-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-dollar-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-dollar-list-atpt)
    (should (eq (char-before) ?$))))

(ert-deftest ar-equalize-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-equalize-list-atpt)
    (should (eq (char-before) ?=))))

(ert-deftest ar-greater-list-angle-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-greaterangle-list-atpt)
    (should (eq (char-before) ?<))))

(ert-deftest ar-lesserangle-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-lesserangle-list-atpt)
    (should (eq (char-before) ?>))))

(ert-deftest ar-backslash-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-backslash-list-atpt)
    (should (eq (char-before) ?\\))))

(ert-deftest ar-backtick-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-backtick-list-atpt)
    (should (eq (char-before) ?`))))

(ert-deftest ar-brace-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-brace-list-atpt)
    (should (eq (char-before) ?}))))

(ert-deftest ar-bracket-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-bracket-list-atpt)
    (should (eq (char-before) ?\]))))

(ert-deftest ar-hyphen-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-hyphen-list-atpt)
    (should (eq (char-before) ?\-))))

(ert-deftest ar-mark-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-mark-list-atpt)
    (should (eq 47 (region-beginning)))
    (should (eq 101 (region-end)))))

(ert-deftest ar-kill-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-kill-list-atpt)
    (should (eq 25 (ar-length-of-list-atpt)))))

(ert-deftest ar-parentize-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-parentize-list-atpt)
    (should (eq (char-before) ?\)))))

(ert-deftest ar-separate-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-separate-list-atpt)
    (should (eolp))))

(ert-deftest ar-singlequote-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-singlequote-list-atpt)
    (should (eq (char-before) ?'))))

(ert-deftest ar-triplequotesq-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-triplequotesq-list-atpt)
    (should (looking-back "'''"))))

(ert-deftest ar-triplequotedq-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-triplequotedq-list-atpt)
    (should (looking-back "\"\"\""))))

(ert-deftest ar-trim-bracelist-atpt-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-trim-list-atpt)
    (should (eq (char-after) 10))))

(ert-deftest ar-left-trim-list-atpt-brace-test ()
  (py-test-with-temp-buffer-point-min
      "feature_operation_matrix = {
    \"character\": {
        \"kill\": \"{ctrl-k}\",{
            }
        }
    }\n"
    (end-of-line)
    (skip-syntax-forward "^(")
    (ar-left-trim-list-atpt)
    (should (eq (char-after) 10))))



(provide 'ar-list-atpt-brace-tests)
