;;; ar-thing-atpt-also-delimited-test.el --- tests succeeding when called interactively  -*- lexical-binding: t; -*-

;; Copyright (C) 2018  Andreas Röhler

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

(ert-deftest ar-delimited-curvedsinglequoted-atpt-test ()
  (ar-test-with-temp-buffer "asdf48"
    (forward-char -1)
    (ar-curvedsinglequote-alnum-atpt)
    (forward-char -1)
    (should (eq 8 (length (ar-delimited-atpt))))))

;; two stars is too much for that decent concept of delimited
;; (ert-deftest ar-delimited-twostarred-test ()
;;   (ar-test-with-temp-buffer
;;       "\*\*foo bar\*\*"
;;       (forward-char -1)
;;     (should (eq 9 (length (ar-delimited-atpt))))
;;     (forward-char -1)
;;     (should (eq 11 (length (ar-delimited-atpt))))
;;     ))

(ert-deftest ar-delimited-curveddoublequoted-atpt-test ()
  (ar-test-with-temp-buffer
      "(or “UQ” for short)"
      (skip-chars-backward "^U")
    (should (eq 4 (length (ar-delimited-atpt))))))

(ert-deftest ar-delimited-coloned-test ()
  (ar-test-with-temp-buffer
      "(defun :foo1: ())"
      (search-backward "1:")
    (should (eq 14 (cdr (ar-bounds-of-delimited-atpt))))))

(ert-deftest ar-delimited2bracket-test-1 ()
  (ar-test-with-temp-buffer
      "(asdf)"
      (forward-char -1)
    (ar-delimited2bracketed-atpt)
    (sit-for 0.1)
    (should (eq 93 (char-after)))))

(ert-deftest kill-delimited-test-1 ()
  (ar-test-with-elisp-buffer
      "(defun foo1 (&optional beg end))\n"
    (goto-char (point-max))
    (search-backward "foo")
    (ar-kill-delimited-atpt)
    (sit-for 0.1)
    (should (eolp))))

;; doc.c
(ert-deftest ar-curved-single-quotes-test ()
    (ar-test-with-elisp-buffer
  "‘like this’"
  (goto-char (point-max)) 
  (forward-char -2)
  (should (eq 11 (length (ar-delimited-atpt))))))

(provide 'ar-thing-atpt-also-delimited-test)
;;; ar-thing-atpt-also-delimited-test.el ends here
