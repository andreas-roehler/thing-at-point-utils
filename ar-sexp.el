;;; ar-sexp.el --- navigate balanced expressions and related stuff -*- lexical-binding: t; -*-

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

(require 'ar-thingatpt-utils)

(defun ar-forward-sexp-intern ()
  (cond ((ignore-errors (ar-delimited-atpt))
         (ar-end-of-delimited-atpt))
        (t (skip-syntax-forward "^(")
           (ar-end-of-delimited-atpt))))

(defun ar-backward-sexp-intern ()
  "Internally used.
Argument CHAR the char before cursor position."
  ;; (let (;;(complement-char (ar--return-complement-char-maybe char))
  ;; (pps (parse-partial-sexp (point-min) (point)))
  ;; (char (ar--return-complement-char-maybe (char-before))))
  (skip-chars-backward " \t\n\r\f")
  (unless (bobp)
    (let ((orig (point)))
      (pcase (char-before)
        (?\" (cond ((and (looking-back "\"\"\"" (line-beginning-position))
                         (save-excursion (forward-char -3) (nth 3 (parse-partial-sexp (point-min) (point)))))
                    (while (and (search-backward (match-string-no-properties 0))
                                (nth 8 (parse-partial-sexp (point-min) (point))))))
                   ((save-excursion (forward-char -1) (nth 3 (parse-partial-sexp (point-min) (point))))
                    (forward-char -1)
                    (goto-char (nth 8 (parse-partial-sexp (point-min) (point)))))
                   (t (unless (bobp)
                        (forward-char -1)
                        (ar-backward-sexp-intern)))))
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
                  (or (member (char-before) th-beg-delimiter-list)
                      (looking-back (concat "[" ar-delimiters-atpt "]") (line-beginning-position)))
                  (cond ((looking-back "\"\"\"\\|'''" (line-beginning-position))
                         (while (and (search-backward (match-string-no-properties 0))
                                     (nth 8 (parse-partial-sexp (point-min) (point))))))
                        (t (save-restriction
                             (narrow-to-region (line-beginning-position) (point))
                             (ar-backward-delimited-atpt))))
                (skip-syntax-backward "^|\s\"\\(\\)" (line-beginning-position))
                ;; (syntax-class-to-char 15)
                )))))
      (and (< (point) orig) (point)))))

(defun ar-forward-sexp (&optional arg)
  "Move forward across one balanced expression (sexp).
Negative ARG -N means move backward across N balanced expressions.

If started from outside a string and comment, do not match inside
and vice versa.

Don't match an opening bracket with closing paren, but ], etc."
  (interactive "P")
  ;; (if (eq 4 (prefix- numeric-value arg))
  ;; (with-temp-buffer
  ;; process code inside string or comment by temporarily removing that property
  (let ((pps (parse-partial-sexp(point-min) (point)))
        erg)
    (if (< 0 (or arg 1))
        (progn
          ;; (when(nth 8 pps)
          ;;   (save-restriction
          ;;     (narrow-to-region
          ;;      (nth 8 pps)(point-max))))
          (ar-forward-sexp-intern))
      ;; (when(nth 8 pps)
      ;;   (save-restriction
      ;;     (narrow-to-region
      ;;      (nth 8 pps) (point))))
      (ar-backward-sexp-intern))))

(defun ar-backward-sexp (&optional arg)
  "Go backward over a balanced expression."
  (interactive "P")
  (ar-forward-sexp -1))


(defvar ar-align-default-re "\\(=>\\|->\\|<-\\|=\\)"
  "Used by ar-align-symbol")

(defun ar-align-equal-sign()
  (interactive "*")
  (ar-align-symbol "="))

(defun ar-align-inline-comment()
  ""
  (interactive "*")
  (save-excursion
    (let ((char (if (search-forward comment-start (line-end-position) t)
                    (progn
                      (goto-char (match-beginning 0))
                      (skip-chars-forward "^[[:blank:]]+")
                      (char-to-string (char-before)))
                  (when (search-backward comment-start (line-beginning-position) t)
                    (char-to-string (char-after))))))
      (ar-align-symbol-intern char))))

(defun ar-align-mode-specific-stop()
  (pcase major-mode
    (`scala-mode (and (eq major-mode 'scala-mode) (looking-at " *case *$")))))

;; *** The two new functions 'looking-at-p' and 'string-match-p' can do
;; the same matching as 'looking-at' and 'string-match' without changing
;; the match data.

(defun ar-align-symbol-intern (regexp)
  ""
  (save-excursion
    (let ((end (copy-marker (line-end-position)))
          (secondcolumn (and
                         (or (looking-at (concat "\\(" regexp "\\)[[:space:]]*"))
                             (looking-back (concat ".+\\(" regexp "\\)[[:space:]]*") (line-beginning-position)))
                         (not
                          (save-excursion
                            (goto-char (match-beginning 1))
                            (save-match-data
                              (looking-back (concat ".+" regexp ".*") (line-beginning-position)))))
                         ;; (member (char-after) (list 32 ?\n ?\t ?\f))
                         ;;  haskell
                         ;; (not (looking-back "main +\\(=+\\) *" (line-beginning-position)))
                         (goto-char (match-beginning 1))
                         ;; better match-data then from looking-back
                         (looking-at regexp)
                         (current-column)))
          (erg (match-string-no-properties 0))
          (maxcolumn 0)
          (indent (current-indentation))
          (orig (copy-marker (point)))
          col)
      ;; an equal-sign at current line
      (when secondcolumn
        ;; (message "(match-string-no-properties 1): %s" (match-string-no-properties 1))
        ;; (setq erg (match-string-no-properties 1))
        ;; (message "erg: %s" erg)
        (setq maxcolumn secondcolumn)
        ;; (setq orig (copy-marker (point)))
        (while
            (and (progn (beginning-of-line)
                        (not (bobp)))
                 (progn (forward-line -1)
                        (not (looking-at comment-start)))
                 (not (ar-empty-line-p))
                 (<= (current-indentation) indent)
                 (or
                  (ar-align-mode-specific-stop)
                  (setq col (string-match erg (buffer-substring-no-properties (line-beginning-position) (line-end-position))))))
          (when col (move-to-column col))
          (when (< maxcolumn (current-column))
            (setq maxcolumn (current-column))))
        ;; before going downward, reach the last match
        ;; (when (match-beginning 1) (goto-char (match-beginning 1)))
        (while (<= (line-end-position) end)
          (setq col (string-match erg (buffer-substring-no-properties (line-beginning-position) (line-end-position))))
          (when col (move-to-column col)
                (when (< (current-column) maxcolumn)
                  (insert (make-string (- maxcolumn (current-column)) 32))))
          (forward-line 1))
        (goto-char orig)
        ;; (when (concat negated-re "+ +\\(" regexp "\\)[[:blank:]]+.*")
        ;;   (when (< (current-column) maxcolumn)
        ;;     (insert (make-string (- maxcolumn (current-column)) 32))))
        ))))

(defun ar-align-symbol (&optional arg)
  (interactive "*p")
  (unless (nth 8 (parse-partial-sexp (point-min) (point)))
    (let ((regexp
           (cond ((stringp arg)
                  arg)
                 ((not (looking-at "[[:alnum:]]"))
                  (char-to-string (char-after)))
                 (t ar-align-default-re))))
      ;; (when t
      (when (or (eq this-command 'self-insert-command) (eq (prefix-numeric-value arg) 1))
        (ar-align-symbol-intern regexp)))))



(defun ar-align-in-current-buffer ()
  (interactive)
  (add-hook 'post-command-hook #'ar-align-symbol nil t))

(provide 'ar-sexp)
;;; ar-sexp.el ends here
