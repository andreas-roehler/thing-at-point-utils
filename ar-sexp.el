;;; ar-sexp.el --- navigate balanced expressions

;; Copyright (C) 2023 Andreas Röhler

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>

;; Version: 0.1

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
;;

(require 'thing-at-point-utils)

;;; Code:

;; (defun ar-sexp ()
;;   "Like `forward-sexp', diffs below.

;; From inside string go to end-of-string.
;; From inside comment, go to end-of-comment.

;; At the end of sexp-same-level, go up if possible.
;; Otherwise return nil."
;;   (interactive)
;;   (let ((orig (point))
;; 	(pps (parse-partial-sexp (point-min) (point)))
;; 	erg)
;;     (cond ((nth 3 pps)
;; 	   (goto-char (nth 8 pps))
;; 	   (forward-sexp))
;; 	  ((or (nth 4 pps)(eq (car (syntax-after (point))) 11))
;; 	   (when (ar-skip-blanks-and-comments nil pps)
;; 	     (ar-sexp)))
;; 	  (t (or (progn (ignore-errors (forward-sexp))
;; 			(< orig (point)))
;; 		 (ignore-errors (up-list)))))
;;     (when (< orig (point)) (setq erg (point)))
;;     erg))

(defun ar-forward-sexp-intern ()
  "Internally used.
Argument CHAR: the char after cursor position."
  (let ((pps (parse-partial-sexp (point-min) (point)))
        (char (ar--return-complement-char-maybe (char-after))))
    (pcase (char-after)
      (?\{ (ar-forward-braced-atpt))
      (?\((ar-forward-parentized-atpt))
      (?\[ (ar-forward-bracketed-atpt))
      (_
       (cond
        ((or (string-match (char-to-string (char-after)) th-beg-delimiter)(looking-at (concat "[" (regexp-quote ar-delimiters-atpt) "]")))
         (ar-forward-delimited-atpt))
        ((nth 3 pps)
	 (goto-char (nth 8 pps))
	 (forward-sexp))
	((or (nth 4 pps)(eq (car (syntax-after (point))) 11))
	 (ar-skip-blanks-and-comments nil pps)))))))

  ;; (when (eq (char-after) char)
  ;;   (unless (eobp) (forward-char 1))))))

(defun ar-backward-sexp-intern ()
  "Internally used.
Argument CHAR the char before cursor position."
  (let (;;(complement-char (ar--return-complement-char-maybe char))
        (pps (parse-partial-sexp (point-min) (point))))
    (cond ((or (nth 3 pps) (nth 4 pps))
	   (goto-char (nth 8 pps)))
          ;; at comment-start
          ;; (eq (car (syntax-after (point))) 11))
	  ;; (ar-skip-blanks-and-comments nil pps))
          (t
           (pcase (char-before)
             (?\} (ar-backward-braced-atpt))
             (?\) (ar-backward-parentized-atpt))
             (?\] (ar-backward-bracketed-atpt))
             (_ (cond ((or (string-match (char-to-string (char-before)) th-beg-delimiter)(looking-back ar-delimiters-atpt (line-beginning-position)))
                       (ar-backward-delimited-atpt)))))))))

(defun ar-forward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
Negative ARG -N means move backward across N balanced expressions.

If started from outside a string and comment, do not match inside and vice versa.

Don't match an opening bracket with closing paren, but ], etc."
  (interactive)
  (if (< 0 (or arg 1))
      (ar-forward-sexp-intern)
    (ar-backward-sexp-intern)))

(defun ar-backward-sexp (&optional arg)
  "Go backward over a balanced expression."
  (interactive)
  (ar-forward-sexp -1))

(provide 'ar-sexp)
;;; ar-sexp.el ends here
