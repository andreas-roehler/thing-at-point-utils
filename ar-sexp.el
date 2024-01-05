;;; ar-sexp.el --- navigate balanced expressions

;; Copyright (C) 2023-2024 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>

;; Version: 0.2

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;; Keywords: convenience

;; (defvar forward-sexp-function 'ar-sexp
;;   "")

;;; Commentary:
;; Similar to ‘forward-sexp’ but, when called from inside a comment or string, match only inside.
;; Likewise, when called from outside a comment or string, ignore these for a match.

(require 'thing-at-point-utils)

(defun ar-forward-sexp-intern ()
  (ignore-errors (ar-forward-delimited-atpt)))

(defun ar-backward-sexp-intern ()
  "Internally used.
Argument CHAR the char before cursor position."
  ;; (let (;;(complement-char (ar--return-complement-char-maybe char))
  ;; (pps (parse-partial-sexp (point-min) (point)))
  ;; (char (ar--return-complement-char-maybe (char-before))))
  (unless (bobp)
    (let ((orig (point)))
      (pcase (char-before)
        (?\" (unless
                 (progn
                   (forward-char -1)
                   (ar-backward-doublequoted-atpt))
               (goto-char orig)))
        (?\} (unless
                 (progn
                   (forward-char -1)
                   (ar-backward-braced-atpt))
               (goto-char orig)))
        (?\) (unless
                 (progn
                   (forward-char -1)
                   (ar-backward-parentized-atpt))
               (goto-char orig)))
        (?\] (unless
                 (progn
                   (forward-char -1)
                   (ar-backward-bracketed-atpt))
               (goto-char orig)))
                (?\{ (forward-char -1)) 

        (?\( (forward-char -1))
             
        (?\[ (forward-char -1))

        (_
         (pcase (char-after)
           (?\" (unless
                    (ar-backward-doublequoted-atpt)
                  (goto-char orig)
                  (unless (bobp)
                    (forward-char -1)
                    (ar-backward-sexp-intern))))
           (?\} (unless
                    (progn
                      ;; (forward-char -1)
                      (ar-backward-braced-atpt))
                  (goto-char orig)))
           (?\) (unless
                    (progn
                      ;; (forward-char -1)
                      (ar-backward-parentized-atpt))
                  (goto-char orig)))
           (?\] (unless
                    (progn
                      ;; (forward-char -1)
                      (ar-backward-bracketed-atpt))
                  (goto-char orig)))

           (_ (if
                  (or
                   (member (char-before) th-beg-delimiter-list)
                   (looking-back (concat "[" ar-delimiters-atpt "]") (line-beginning-position))
                   ;; (ar-in-delimited-p)
                   )
                  ;; at-point functions work from cursor position
                  ;; (forward-char -1)
                  (ar-backward-delimited-atpt)
                (backward-word))))))
      (and (< (point) orig) (point)))))

(defun ar-forward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
Negative ARG -N means move backward across N balanced expressions.

If started from outside a string and comment, do not match inside
and vice versa.

Don't match an opening bracket with closing paren, but ], etc."
  (interactive "P")
  ;; (if (eq 4 (prefix-numeric-value arg))
      ;; (with-temp-buffer
      ;; process code inside string or comment by temporarily removing that property
  (let ((pps (parse-partial-sexp (point-min) (point)))
        erg)
    (if (nth 8 pps)
        (save-restriction
          (save-excursion
            (goto-char (nth 8 pps))
            (narrow-to-region
             (point)
             (if (nth 3 pps)
                 (progn (forward-sexp) (point))
               (progn
                 (forward-comment 1)
                 (point))
)))
          (if (< 0 (or arg 1))
              (ar-forward-sexp-intern)
            (ar-backward-sexp-intern)))
      (if (< 0 (or arg 1))
          (ar-forward-sexp-intern)
        (ar-backward-sexp-intern)))))

(defun ar-backward-sexp (&optional arg)
  "Go backward over a balanced expression."
  (interactive "P")
  (ar-forward-sexp -1))

(provide 'ar-sexp)
;;; ar-sexp.el ends here
