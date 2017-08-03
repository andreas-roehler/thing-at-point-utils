;;; ar-peel-tests.el --- test thingatpt-utils peel forms

;; Copyright (C) 2011  Andreas Roehler

;; Author: Andreas Roehler <andreas.roehler@online.de>
;; Keywords: convenience, languages

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

;;; Commentary: Only `peel-list'  is implemented

;;

;;; Code:

(defvar ar-thingatpt-peel-tests nil
  "Test thingatpt-utils peel forms. ")

(setq ar-thingatpt-peel-tests (list
                               'ar-peel-parentized-atpt-test
                               'ar-peel-bracketed-atpt-test))


(defun ar-run-thingatpt-peel-tests (&optional liste)
  (interactive)
  (let ((liste (or liste ar-thingatpt-peel-tests)))
    (dolist (ele liste)
      (funcall ele))))

(ert-deftest ar-ert-peel-parentized-test ()
  (ar-test-with-elisp-buffer-point-min
   "(foo (bar baz))"
   (forward-char 1)
   (ar-peel-parentized-atpt)
   (should (looking-at "(bar baz)"))))

(defun ar-peel-parentized-atpt-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "(foo (bar baz))"))
  (py-bug-tests-intern 'ar-peel-parentized-test-base arg teststring)))

(defun ar-peel-parentized-test-base ()
    (goto-char (point-min))
    (forward-char 1)
    (ar-peel-parentized-atpt)
    (assert (looking-at "(bar baz)") nil "ar-peel-parentized-atpt-test failed"))

(defun ar-peel-bracketed-atpt-test (&optional arg load-branch-function)
  (interactive "p")
  (let ((teststring "[foo [bar baz]]"))
  (py-bug-tests-intern 'ar-peel-bracketed-test-base arg teststring)))

(defun ar-peel-bracketed-test-base ()
    (goto-char (point-min))
    (forward-char 1)
    (ar-peel-bracketed-atpt)
    (assert (looking-at "\\[bar baz]") nil "ar-peel-parentized-atpt-test failed"))


(provide 'ar-peel-tests)
;;; ar-peel-tests.el ends here
