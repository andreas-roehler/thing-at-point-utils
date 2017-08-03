;;; thing-at-point-utils-tests.el --- -*- lexical-binding: t; -*- 

;; Copyright (C) 2014-2017  Andreas Roehler

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

(require 'ar-thing-at-point-utils-setup-tests)

;; moved to ar-werkstatt-interactive-tests.el
;; as failing from shell inly

(ert-deftest ar-in-doublequoted-test ()
  (ar-test-with-temp-buffer
      "\"
;;;\" \"Write 'etc. \""
      (emacs-lisp-mode)
    (goto-char 2)
    (let ((erg (ar-doublequoted-atpt)))
      (should (eq 6 (length erg))))
    (goto-char 3)
    (let ((erg (ar-doublequoted-atpt)))
      (should (eq 6 (length erg))))
    (goto-char 9)
    (let ((erg (ar-doublequoted-atpt)))
      (should (eq 14 (length erg))))))

(ert-deftest ar-in-comment-p-atpt-test ()
  (ar-test-with-temp-buffer
      "
\"
;;;\""
      (emacs-lisp-mode)
      (should (not (ar-in-comment-p-atpt)))))

(ert-deftest ar-beginning-of-list-atpt-test ()
  (ar-test-with-elisp-buffer-point-min
      "(car (cons 1 2))"
    (forward-char 1)
    (skip-chars-forward "^(")
    (should (eq 6 (ar-beginning-of-list-atpt)))))

(provide 'thing-at-point-utils-tests)
;;; thing-at-point-utils-tests.el ends here
