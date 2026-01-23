;;; ar-thingatpt-peel-tests.el --- -*- lexical-binding: t; -*-

;; Copyright (C) 2014-2026  Andreas Roehler

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

(ert-deftest ar-peel-test-szpCWi ()
  (ar-test-point-min
      "(defun foo1 (&optional beg end))"
    'emacs-lisp-mode
    'ar-verbose-p
    (goto-char (point-min))
    (ar-peel-list-atpt)
    (should (looking-back "(&optional beg end)" (line-beginning-position)))))

(ert-deftest ar-peel-test-MumEC3 ()
  (ar-test
      "(defun foo1 (&optional beg end))"
    'emacs-lisp-mode
    'ar-verbose-p
    (goto-char (point-max))
    (forward-char -1)
    (ar-peel-list-atpt)
    (sit-for 0.1)
    (should (looking-back "(&optional beg end)" (line-beginning-position)))))

(ert-deftest ar-peel-list-atpt-test-yAeeNz ()
  (ar-test-with-elisp-buffer-point-min
	"(defun foo ()
  (let ((bar (save-excursion (baz nil nil))))
    (setq asdf nil)))"
	(search-forward "save-")
      ;; (sit-for 0.1)
      (ar-peel-list-atpt)
      ;; (sit-for 0.1)
      (should (looking-back "(let ((bar (baz nil nil)" (line-beginning-position)1)))
  )

;; (ert-deftest ar-peel-list-with-string-test-HhDRbb ()
;;   (ar-test-with-elisp-buffer-point-min
;;       "(expand-file-name \"~/werkstatt/general-key\")"
;;       (forward-char 2)
;;     (ar-peel-list-atpt)
;;     (forward-char 1)
;;     (should-not (nth 1 (parse-partial-sexp (point-min) (point))))
;;     (should (nth 3 (parse-partial-sexp (point-min) (point))))))

(provide 'ar-thingatpt-peel-tests)
;;; ar-thingatpt-peel-tests.el ends here
