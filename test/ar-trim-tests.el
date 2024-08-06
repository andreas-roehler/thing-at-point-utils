;;; ar-trim-tests.el --- test trim commands           -*- lexical-binding: t; -*-

;; Copyright (C) 2016-2024  Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>
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

(ert-deftest ar-trim-braced-in-region-test ()
  (ar-test-with-elisp-buffer-point-min
      "{asdf} {asdf} {asdf}"
      (goto-char (point-min))
      (set-mark (point))
    (goto-char (point-max))
    (ar-trim-braced-in-region-atpt)
    (should (eq (char-before) ?f))))

;; (ert-deftest ar-peel-list-with-string-test-1 ()
;;   (ar-test-with-elisp-buffer-point-min
;;       "(expand-file-name \"~/werkstatt/general-key\")"
;;       (forward-char 2)
;;     (ar-peel-list-atpt)
;;     (forward-char 1)
;;     (should-not (nth 1 (parse-partial-sexp (point-min) (point))))
;;     (should (nth 3 (parse-partial-sexp (point-min) (point))))))

(ert-deftest ar-bounds-of-list-with-string-test-1 ()
  (ar-test-with-elisp-buffer-point-min
      "(expand-file-name \"~/werkstatt/general-key\")"
      "(defun foo1 (&optional beg end)
  \" \"
  (interactive \"\*\")
  (let ((beg (cond (beg)
                   ((use-region-p)
                    (region-beginning))
                   (t (point-min))))
        (end (cond (end (copy-marker end))
                   ((use-region-p)
                    (copy-marker (region-end)))
                   (t (copy-marker (point-max))))))
    (save-excursion
      (goto-char beg))
    (when (interactive-p) (message \"%s %s\" beg end))))"
    (should (eq 1 (car (ar-bounds-of-list-atpt))))))

(provide 'ar-trim-tests)
;;; ar-trim-tests.el ends here
