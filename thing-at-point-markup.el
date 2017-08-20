;;; thing-at-point-markup.el --- thing-at-point utils

;; Copyright (C) 2010-2017 Andreas Röhler, unless
;; indicated otherwise

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

;; Version: 0.1

;; Keywords: convenience

;; This file is free software; you can redistribute it
;; and/or modify it under the terms of the GNU General Public License as published by the Free Software Foundation; either version 2, or (at your option) any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;;; Code:

(require 'thingatpt-utils-core)

;; ar-thing-at-point-utils-markup ar-atpt-markup-list start

(defun ar-un-beginendquote-atpt (&optional arg)
  "Remove markups provided by beginendquote. "
  (interactive "P*")
  (ar-th-un-ml 'beginendquote arg (interactive-p)))

(defun ar-un-blok-atpt (&optional arg)
  "Remove markups provided by blok. "
  (interactive "P*")
  (ar-th-un-ml 'blok arg (interactive-p)))

(defun ar-un-doublebackslashed-atpt (&optional arg)
  "Remove markups provided by doublebackslashed. "
  (interactive "P*")
  (ar-th-un-ml 'doublebackslashed arg (interactive-p)))

(defun ar-un-doublebackslashedparen-atpt (&optional arg)
  "Remove markups provided by doublebackslashedparen. "
  (interactive "P*")
  (ar-th-un-ml 'doublebackslashedparen arg (interactive-p)))

(defun ar-un-doubleslashed-atpt (&optional arg)
  "Remove markups provided by doubleslashed. "
  (interactive "P*")
  (ar-th-un-ml 'doubleslashed arg (interactive-p)))

(defun ar-un-tabledata-atpt (&optional arg)
  "Remove markups provided by tabledata. "
  (interactive "P*")
  (ar-th-un-ml 'tabledata arg (interactive-p)))

;; ar-thing-at-point-utils-markup: ar-atpt-markup-list end


;; ar-thing-at-point-utils-delimiters-core ar-atpt-markup-list start

(defun ar-beginendquote-atpt (&optional arg no-delimiters)
  "Returns beginendquote at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'beginendquote arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-beginendquote-atpt 'ar-beginendquote-bounds-atpt)
(defun ar-beginendquote-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of beginendquote if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'beginendquote no-delimiters (called-interactively-p 'any)))

(defun ar-beginendquote-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BEGINENDQUOTE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'beginendquote no-delimiters (called-interactively-p 'any)))

(defun ar-beginendquote-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BEGINENDQUOTE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-beginendquote-atpt 'ar-beginendquote-beginning-atpt)
(defun ar-beginendquote-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class BEGINENDQUOTE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-beginendquote-atpt 'ar-beginendquote-end-atpt)
(defun ar-beginendquote-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class BEGINENDQUOTE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-beginendquote-p-atpt 'ar-beginendquote-in-p-atpt)
(defun ar-beginendquote-in-p-atpt (&optional no-delimiters)
  "Returns bounds of BEGINENDQUOTE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-beginendquote-atpt 'ar-beginendquote-length-atpt)
(defun ar-beginendquote-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class BEGINENDQUOTE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-beginendquote-atpt 'ar-beginendquote-copy-atpt)
(defun ar-beginendquote-copy-atpt (&optional no-delimiters)
  "Returns a copy of BEGINENDQUOTE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-beginendquote-atpt 'ar-beginendquote-delete-atpt)
(defun ar-beginendquote-delete-atpt (&optional arg)
  "Deletes BEGINENDQUOTE at point if any. "
  (interactive "*p")
  (ar-th-delete 'beginendquote arg (called-interactively-p 'any)))

(defalias 'ar-delete-beginendquote-in-region 'ar-beginendquote-delete-in-region)
(defun ar-beginendquote-delete-in-region (beg end)
  "Deletes BEGINENDQUOTE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'beginendquote beg end (called-interactively-p 'any)))

(defun ar-blok-beginendquote-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around beginendquote.
  Returns blok or nil if no BEGINENDQUOTE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-beginendquote-atpt 'ar-beginendquote-backslashparen-atpt)
(defun ar-backslashparen-beginendquote-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around beginendquote at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-beginendquote-atpt 'ar-beginendquote-doublebackslash-atpt)
(defun ar-beginendquote-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BEGINENDQUOTE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-beginendquote-atpt 'ar-beginendquote-doubleslash-atpt)
(defun ar-beginendquote-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around BEGINENDQUOTE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-beginendquote-atpt 'ar-beginendquote-doublebackslashparen-atpt)
(defun ar-beginendquote-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BEGINENDQUOTE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-beginendquote-atpt 'ar-beginendquote-slashparen-atpt)
(defun ar-beginendquote-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around BEGINENDQUOTE at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'beginendquote no-delimiters (called-interactively-p 'any)))

(defun ar-comment-beginendquote-atpt (&optional no-delimiters)
  "Comments BEGINENDQUOTE at point if any. "
  (interactive "*p")
  (ar-th-comment 'beginendquote no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-beginendquote-atpt (&optional no-delimiters)
  "Put a comma after BEGINENDQUOTE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'beginendquote no-delimiters (called-interactively-p 'any)))

(defun ar-quote-beginendquote-atpt (&optional no-delimiters)
  "Put a singlequote before BEGINENDQUOTE at point if any. "
  (interactive "*p")
  (ar-th-quote 'beginendquote no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-beginendquote-atpt 'ar-beginendquote-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-beginendquote-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around BEGINENDQUOTE at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-beginendquote-atpt 'ar-beginendquote-mark-atpt)
(defun ar-beginendquote-mark-atpt ()
  "Marks BEGINENDQUOTE at point if any. "
  (interactive)
  (ar-th-mark 'beginendquote))

(defalias 'ar-hide-beginendquote-atpt 'ar-beginendquote-hide-atpt)
(defun ar-beginendquote-hide-atpt ()
  "Hides BEGINENDQUOTE at point. "
  (interactive)
  (ar-th-hide 'beginendquote))

(defalias 'ar-show-beginendquote-atpt 'ar-beginendquote-show-atpt)
(defun ar-beginendquote-show-atpt ()
  "Shows hidden BEGINENDQUOTE at point. "
  (interactive)
  (ar-th-show 'beginendquote))

(defalias 'ar-hide-show-beginendquote-atpt 'ar-beginendquote-hide-show-atpt)
(defun ar-beginendquote-hide-show-atpt ()
  "Alternatively hides or shows BEGINENDQUOTE at point. "
  (interactive)
  (ar-th-hide-show 'beginendquote))

(defalias 'ar-highlight-beginendquote-atpt-mode 'ar-beginendquote-highlight-atpt-mode)

(defun ar-beginendquote-highlight-atpt-mode (&optional no-delimiters)
  "Toggles beginendquote-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-beginendquote-atpt 'ar-beginendquote-kill-atpt)
(defun ar-beginendquote-kill-atpt (&optional no-delimiters)
  "Kills BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-kill 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-beginendquote-atpt 'ar-beginendquote-kill-backward-atpt)
(defun ar-beginendquote-kill-backward-atpt (&optional no-delimiters)
  "Kills BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-beginendquote-atpt 'ar-beginendquote-leftrightsinglequote-atpt)
(defun ar-beginendquote-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'beginendquote no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-beginendquote-atpt 'ar-beginendquote-parentize-atpt)
;; ;;;###autoload
;; (defun ar-beginendquote-parentize-atpt (&optional no-delimiters)
;;   "Parentizes BEGINENDQUOTE at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-beginendquote-atpt 'ar-beginendquote-separate-atpt)
(defun ar-beginendquote-separate-atpt (&optional no-delimiters)
  "Separates BEGINENDQUOTE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'beginendquote no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-beginendquote-atpt 'ar-beginendquote-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-beginendquote-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes BEGINENDQUOTE at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-beginendquote-atpt 'ar-beginendquote-triplequotedq-atpt)
(defun ar-beginendquote-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around beginendquote. "
  (interactive "*p")
  (ar-th-triplequotedq 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-beginendquote-atpt 'ar-beginendquote-triplequotesq-atpt)
(defun ar-beginendquote-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around beginendquote. "
  (interactive "*p")
  (ar-th-triplequotesq 'beginendquote no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginendquote-trim-atpt 'ar-trim-beginendquote-atpt)
(defun ar-trim-beginendquote-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'beginendquote t t))

(defalias 'ar-trim-left-beginendquote-atpt 'ar-trim-beginendquote-left-atpt)
(defun ar-trim-beginendquote-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'beginendquote t nil))

(defalias 'ar-trim-right-beginendquote-atpt 'ar-trim-beginendquote-right-atpt)
(defun ar-trim-beginendquote-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'beginendquote nil t))

(defun ar-underscore-beginendquote-atpt (&optional no-delimiters)
  "Put underscore char around BEGINENDQUOTE. "
  (interactive "*p")
  (ar-th-underscore 'beginendquote no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-beginendquote-whitespace-atpt 'ar-whitespace-beginendquote-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-beginendquote-atpt (&optional no-delimiters)
;;   "Put whitespace char around BEGINENDQUOTE. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'beginendquote nil t))

(defalias 'ar-forward-beginendquote-atpt 'ar-beginendquote-forward-atpt)
(defun ar-beginendquote-forward-atpt (&optional arg)
  "Moves forward over BEGINENDQUOTE at point if any, does nothing otherwise.
Returns end position of BEGINENDQUOTE "
  (interactive "p")
  (ar-th-forward 'beginendquote arg (called-interactively-p 'any)))

(defalias 'ar-backward-beginendquote-atpt 'ar-beginendquote-backward-atpt)
(defun ar-beginendquote-backward-atpt (&optional arg)
  "Moves backward over BEGINENDQUOTE before point if any, does nothing otherwise.
Returns beginning position of BEGINENDQUOTE "
  (interactive "p")
  (ar-th-backward 'beginendquote arg (called-interactively-p 'any)))

(defalias 'ar-transpose-beginendquote-atpt 'ar-beginendquote-transpose-atpt)
(defun ar-beginendquote-transpose-atpt (&optional arg)
  "Transposes BEGINENDQUOTE with BEGINENDQUOTE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'beginendquote arg (called-interactively-p 'any)))

(defalias 'ar-sort-beginendquote-atpt 'ar-beginendquote-sort-atpt)
(defun ar-beginendquote-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts beginendquotes in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'beginendquote reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-beginendquote-atpt 'ar-beginendquote-check-atpt)
(defun ar-beginendquote-check-atpt ()
  "Return t if a BEGINENDQUOTE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-beginendquote-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-beginendquote-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-blok-atpt (&optional arg no-delimiters)
  "Returns blok at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'blok arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-blok-atpt 'ar-blok-bounds-atpt)
(defun ar-blok-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of blok if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'blok no-delimiters (called-interactively-p 'any)))

(defun ar-blok-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BLOK at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'blok no-delimiters (called-interactively-p 'any)))

(defun ar-blok-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BLOK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-blok-atpt 'ar-blok-beginning-atpt)
(defun ar-blok-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class BLOK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-blok-atpt 'ar-blok-end-atpt)
(defun ar-blok-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class BLOK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-blok-p-atpt 'ar-blok-in-p-atpt)
(defun ar-blok-in-p-atpt (&optional no-delimiters)
  "Returns bounds of BLOK at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-blok-atpt 'ar-blok-length-atpt)
(defun ar-blok-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class BLOK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-blok-atpt 'ar-blok-copy-atpt)
(defun ar-blok-copy-atpt (&optional no-delimiters)
  "Returns a copy of BLOK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-blok-atpt 'ar-blok-delete-atpt)
(defun ar-blok-delete-atpt (&optional arg)
  "Deletes BLOK at point if any. "
  (interactive "*p")
  (ar-th-delete 'blok arg (called-interactively-p 'any)))

(defalias 'ar-delete-blok-in-region 'ar-blok-delete-in-region)
(defun ar-blok-delete-in-region (beg end)
  "Deletes BLOK at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'blok beg end (called-interactively-p 'any)))

(defun ar-blok-blok-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around blok.
  Returns blok or nil if no BLOK at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-blok-atpt 'ar-blok-backslashparen-atpt)
(defun ar-backslashparen-blok-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around blok at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-blok-atpt 'ar-blok-doublebackslash-atpt)
(defun ar-blok-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BLOK at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-blok-atpt 'ar-blok-doubleslash-atpt)
(defun ar-blok-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around BLOK at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-blok-atpt 'ar-blok-doublebackslashparen-atpt)
(defun ar-blok-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BLOK at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-blok-atpt 'ar-blok-slashparen-atpt)
(defun ar-blok-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around BLOK at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'blok no-delimiters (called-interactively-p 'any)))

(defun ar-comment-blok-atpt (&optional no-delimiters)
  "Comments BLOK at point if any. "
  (interactive "*p")
  (ar-th-comment 'blok no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-blok-atpt (&optional no-delimiters)
  "Put a comma after BLOK at point if any. "
  (interactive "*p")
  (ar-th-commatize 'blok no-delimiters (called-interactively-p 'any)))

(defun ar-quote-blok-atpt (&optional no-delimiters)
  "Put a singlequote before BLOK at point if any. "
  (interactive "*p")
  (ar-th-quote 'blok no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-blok-atpt 'ar-blok-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-blok-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around BLOK at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-blok-atpt 'ar-blok-mark-atpt)
(defun ar-blok-mark-atpt ()
  "Marks BLOK at point if any. "
  (interactive)
  (ar-th-mark 'blok))

(defalias 'ar-hide-blok-atpt 'ar-blok-hide-atpt)
(defun ar-blok-hide-atpt ()
  "Hides BLOK at point. "
  (interactive)
  (ar-th-hide 'blok))

(defalias 'ar-show-blok-atpt 'ar-blok-show-atpt)
(defun ar-blok-show-atpt ()
  "Shows hidden BLOK at point. "
  (interactive)
  (ar-th-show 'blok))

(defalias 'ar-hide-show-blok-atpt 'ar-blok-hide-show-atpt)
(defun ar-blok-hide-show-atpt ()
  "Alternatively hides or shows BLOK at point. "
  (interactive)
  (ar-th-hide-show 'blok))

(defalias 'ar-highlight-blok-atpt-mode 'ar-blok-highlight-atpt-mode)

(defun ar-blok-highlight-atpt-mode (&optional no-delimiters)
  "Toggles blok-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-blok-atpt 'ar-blok-kill-atpt)
(defun ar-blok-kill-atpt (&optional no-delimiters)
  "Kills BLOK at point if any. "
  (interactive "*P")
  (ar-th-kill 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-blok-atpt 'ar-blok-kill-backward-atpt)
(defun ar-blok-kill-backward-atpt (&optional no-delimiters)
  "Kills BLOK at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-blok-atpt 'ar-blok-leftrightsinglequote-atpt)
(defun ar-blok-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'blok no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-blok-atpt 'ar-blok-parentize-atpt)
;; ;;;###autoload
;; (defun ar-blok-parentize-atpt (&optional no-delimiters)
;;   "Parentizes BLOK at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-blok-atpt 'ar-blok-separate-atpt)
(defun ar-blok-separate-atpt (&optional no-delimiters)
  "Separates BLOK at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'blok no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-blok-atpt 'ar-blok-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-blok-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes BLOK at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-blok-atpt 'ar-blok-triplequotedq-atpt)
(defun ar-blok-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around blok. "
  (interactive "*p")
  (ar-th-triplequotedq 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-blok-atpt 'ar-blok-triplequotesq-atpt)
(defun ar-blok-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around blok. "
  (interactive "*p")
  (ar-th-triplequotesq 'blok no-delimiters (called-interactively-p 'any)))

(defalias 'ar-blok-trim-atpt 'ar-trim-blok-atpt)
(defun ar-trim-blok-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'blok t t))

(defalias 'ar-trim-left-blok-atpt 'ar-trim-blok-left-atpt)
(defun ar-trim-blok-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'blok t nil))

(defalias 'ar-trim-right-blok-atpt 'ar-trim-blok-right-atpt)
(defun ar-trim-blok-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'blok nil t))

(defun ar-underscore-blok-atpt (&optional no-delimiters)
  "Put underscore char around BLOK. "
  (interactive "*p")
  (ar-th-underscore 'blok no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-blok-whitespace-atpt 'ar-whitespace-blok-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-blok-atpt (&optional no-delimiters)
;;   "Put whitespace char around BLOK. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'blok nil t))

(defalias 'ar-forward-blok-atpt 'ar-blok-forward-atpt)
(defun ar-blok-forward-atpt (&optional arg)
  "Moves forward over BLOK at point if any, does nothing otherwise.
Returns end position of BLOK "
  (interactive "p")
  (ar-th-forward 'blok arg (called-interactively-p 'any)))

(defalias 'ar-backward-blok-atpt 'ar-blok-backward-atpt)
(defun ar-blok-backward-atpt (&optional arg)
  "Moves backward over BLOK before point if any, does nothing otherwise.
Returns beginning position of BLOK "
  (interactive "p")
  (ar-th-backward 'blok arg (called-interactively-p 'any)))

(defalias 'ar-transpose-blok-atpt 'ar-blok-transpose-atpt)
(defun ar-blok-transpose-atpt (&optional arg)
  "Transposes BLOK with BLOK before point if any. "
  (interactive "*p")
  (ar-th-transpose 'blok arg (called-interactively-p 'any)))

(defalias 'ar-sort-blok-atpt 'ar-blok-sort-atpt)
(defun ar-blok-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts bloks in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'blok reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-blok-atpt 'ar-blok-check-atpt)
(defun ar-blok-check-atpt ()
  "Return t if a BLOK at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-blok-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-blok-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-doublebackslashed-atpt (&optional arg no-delimiters)
  "Returns doublebackslashed at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'doublebackslashed arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-doublebackslashed-atpt 'ar-doublebackslashed-bounds-atpt)
(defun ar-doublebackslashed-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of doublebackslashed if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defun ar-doublebackslashed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLEBACKSLASHED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defun ar-doublebackslashed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLEBACKSLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-doublebackslashed-atpt 'ar-doublebackslashed-beginning-atpt)
(defun ar-doublebackslashed-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DOUBLEBACKSLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-doublebackslashed-atpt 'ar-doublebackslashed-end-atpt)
(defun ar-doublebackslashed-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DOUBLEBACKSLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-doublebackslashed-p-atpt 'ar-doublebackslashed-in-p-atpt)
(defun ar-doublebackslashed-in-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLEBACKSLASHED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-doublebackslashed-atpt 'ar-doublebackslashed-length-atpt)
(defun ar-doublebackslashed-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DOUBLEBACKSLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-doublebackslashed-atpt 'ar-doublebackslashed-copy-atpt)
(defun ar-doublebackslashed-copy-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLEBACKSLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-doublebackslashed-atpt 'ar-doublebackslashed-delete-atpt)
(defun ar-doublebackslashed-delete-atpt (&optional arg)
  "Deletes DOUBLEBACKSLASHED at point if any. "
  (interactive "*p")
  (ar-th-delete 'doublebackslashed arg (called-interactively-p 'any)))

(defalias 'ar-delete-doublebackslashed-in-region 'ar-doublebackslashed-delete-in-region)
(defun ar-doublebackslashed-delete-in-region (beg end)
  "Deletes DOUBLEBACKSLASHED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doublebackslashed beg end (called-interactively-p 'any)))

(defun ar-blok-doublebackslashed-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around doublebackslashed.
  Returns blok or nil if no DOUBLEBACKSLASHED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-doublebackslashed-atpt 'ar-doublebackslashed-backslashparen-atpt)
(defun ar-backslashparen-doublebackslashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doublebackslashed at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-doublebackslashed-atpt 'ar-doublebackslashed-doublebackslash-atpt)
(defun ar-doublebackslashed-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLEBACKSLASHED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-doublebackslashed-atpt 'ar-doublebackslashed-doubleslash-atpt)
(defun ar-doublebackslashed-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLEBACKSLASHED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-doublebackslashed-atpt 'ar-doublebackslashed-doublebackslashparen-atpt)
(defun ar-doublebackslashed-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLEBACKSLASHED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-doublebackslashed-atpt 'ar-doublebackslashed-slashparen-atpt)
(defun ar-doublebackslashed-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLEBACKSLASHED at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defun ar-comment-doublebackslashed-atpt (&optional no-delimiters)
  "Comments DOUBLEBACKSLASHED at point if any. "
  (interactive "*p")
  (ar-th-comment 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-doublebackslashed-atpt (&optional no-delimiters)
  "Put a comma after DOUBLEBACKSLASHED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defun ar-quote-doublebackslashed-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLEBACKSLASHED at point if any. "
  (interactive "*p")
  (ar-th-quote 'doublebackslashed no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-doublebackslashed-atpt 'ar-doublebackslashed-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-doublebackslashed-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around DOUBLEBACKSLASHED at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-doublebackslashed-atpt 'ar-doublebackslashed-mark-atpt)
(defun ar-doublebackslashed-mark-atpt ()
  "Marks DOUBLEBACKSLASHED at point if any. "
  (interactive)
  (ar-th-mark 'doublebackslashed))

(defalias 'ar-hide-doublebackslashed-atpt 'ar-doublebackslashed-hide-atpt)
(defun ar-doublebackslashed-hide-atpt ()
  "Hides DOUBLEBACKSLASHED at point. "
  (interactive)
  (ar-th-hide 'doublebackslashed))

(defalias 'ar-show-doublebackslashed-atpt 'ar-doublebackslashed-show-atpt)
(defun ar-doublebackslashed-show-atpt ()
  "Shows hidden DOUBLEBACKSLASHED at point. "
  (interactive)
  (ar-th-show 'doublebackslashed))

(defalias 'ar-hide-show-doublebackslashed-atpt 'ar-doublebackslashed-hide-show-atpt)
(defun ar-doublebackslashed-hide-show-atpt ()
  "Alternatively hides or shows DOUBLEBACKSLASHED at point. "
  (interactive)
  (ar-th-hide-show 'doublebackslashed))

(defalias 'ar-highlight-doublebackslashed-atpt-mode 'ar-doublebackslashed-highlight-atpt-mode)

(defun ar-doublebackslashed-highlight-atpt-mode (&optional no-delimiters)
  "Toggles doublebackslashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-doublebackslashed-atpt 'ar-doublebackslashed-kill-atpt)
(defun ar-doublebackslashed-kill-atpt (&optional no-delimiters)
  "Kills DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-kill 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-doublebackslashed-atpt 'ar-doublebackslashed-kill-backward-atpt)
(defun ar-doublebackslashed-kill-backward-atpt (&optional no-delimiters)
  "Kills DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-doublebackslashed-atpt 'ar-doublebackslashed-leftrightsinglequote-atpt)
(defun ar-doublebackslashed-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'doublebackslashed no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-doublebackslashed-atpt 'ar-doublebackslashed-parentize-atpt)
;; ;;;###autoload
;; (defun ar-doublebackslashed-parentize-atpt (&optional no-delimiters)
;;   "Parentizes DOUBLEBACKSLASHED at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-doublebackslashed-atpt 'ar-doublebackslashed-separate-atpt)
(defun ar-doublebackslashed-separate-atpt (&optional no-delimiters)
  "Separates DOUBLEBACKSLASHED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'doublebackslashed no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-doublebackslashed-atpt 'ar-doublebackslashed-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-doublebackslashed-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes DOUBLEBACKSLASHED at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-doublebackslashed-atpt 'ar-doublebackslashed-triplequotedq-atpt)
(defun ar-doublebackslashed-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doublebackslashed. "
  (interactive "*p")
  (ar-th-triplequotedq 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-doublebackslashed-atpt 'ar-doublebackslashed-triplequotesq-atpt)
(defun ar-doublebackslashed-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doublebackslashed. "
  (interactive "*p")
  (ar-th-triplequotesq 'doublebackslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashed-trim-atpt 'ar-trim-doublebackslashed-atpt)
(defun ar-trim-doublebackslashed-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashed t t))

(defalias 'ar-trim-left-doublebackslashed-atpt 'ar-trim-doublebackslashed-left-atpt)
(defun ar-trim-doublebackslashed-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashed t nil))

(defalias 'ar-trim-right-doublebackslashed-atpt 'ar-trim-doublebackslashed-right-atpt)
(defun ar-trim-doublebackslashed-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashed nil t))

(defun ar-underscore-doublebackslashed-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLEBACKSLASHED. "
  (interactive "*p")
  (ar-th-underscore 'doublebackslashed no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-doublebackslashed-whitespace-atpt 'ar-whitespace-doublebackslashed-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-doublebackslashed-atpt (&optional no-delimiters)
;;   "Put whitespace char around DOUBLEBACKSLASHED. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'doublebackslashed nil t))

(defalias 'ar-forward-doublebackslashed-atpt 'ar-doublebackslashed-forward-atpt)
(defun ar-doublebackslashed-forward-atpt (&optional arg)
  "Moves forward over DOUBLEBACKSLASHED at point if any, does nothing otherwise.
Returns end position of DOUBLEBACKSLASHED "
  (interactive "p")
  (ar-th-forward 'doublebackslashed arg (called-interactively-p 'any)))

(defalias 'ar-backward-doublebackslashed-atpt 'ar-doublebackslashed-backward-atpt)
(defun ar-doublebackslashed-backward-atpt (&optional arg)
  "Moves backward over DOUBLEBACKSLASHED before point if any, does nothing otherwise.
Returns beginning position of DOUBLEBACKSLASHED "
  (interactive "p")
  (ar-th-backward 'doublebackslashed arg (called-interactively-p 'any)))

(defalias 'ar-transpose-doublebackslashed-atpt 'ar-doublebackslashed-transpose-atpt)
(defun ar-doublebackslashed-transpose-atpt (&optional arg)
  "Transposes DOUBLEBACKSLASHED with DOUBLEBACKSLASHED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'doublebackslashed arg (called-interactively-p 'any)))

(defalias 'ar-sort-doublebackslashed-atpt 'ar-doublebackslashed-sort-atpt)
(defun ar-doublebackslashed-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts doublebackslasheds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'doublebackslashed reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-doublebackslashed-atpt 'ar-doublebackslashed-check-atpt)
(defun ar-doublebackslashed-check-atpt ()
  "Return t if a DOUBLEBACKSLASHED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-doublebackslashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doublebackslashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-doublebackslashedparen-atpt (&optional arg no-delimiters)
  "Returns doublebackslashedparen at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'doublebackslashedparen arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-doublebackslashedparen-atpt 'ar-doublebackslashedparen-bounds-atpt)
(defun ar-doublebackslashedparen-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of doublebackslashedparen if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-doublebackslashedparen-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-doublebackslashedparen-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-doublebackslashedparen-atpt 'ar-doublebackslashedparen-beginning-atpt)
(defun ar-doublebackslashedparen-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-doublebackslashedparen-atpt 'ar-doublebackslashedparen-end-atpt)
(defun ar-doublebackslashedparen-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-doublebackslashedparen-p-atpt 'ar-doublebackslashedparen-in-p-atpt)
(defun ar-doublebackslashedparen-in-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLEBACKSLASHEDPAREN at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-doublebackslashedparen-atpt 'ar-doublebackslashedparen-length-atpt)
(defun ar-doublebackslashedparen-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-doublebackslashedparen-atpt 'ar-doublebackslashedparen-copy-atpt)
(defun ar-doublebackslashedparen-copy-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-doublebackslashedparen-atpt 'ar-doublebackslashedparen-delete-atpt)
(defun ar-doublebackslashedparen-delete-atpt (&optional arg)
  "Deletes DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-delete 'doublebackslashedparen arg (called-interactively-p 'any)))

(defalias 'ar-delete-doublebackslashedparen-in-region 'ar-doublebackslashedparen-delete-in-region)
(defun ar-doublebackslashedparen-delete-in-region (beg end)
  "Deletes DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doublebackslashedparen beg end (called-interactively-p 'any)))

(defun ar-blok-doublebackslashedparen-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around doublebackslashedparen.
  Returns blok or nil if no DOUBLEBACKSLASHEDPAREN at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-doublebackslashedparen-atpt 'ar-doublebackslashedparen-backslashparen-atpt)
(defun ar-backslashparen-doublebackslashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doublebackslashedparen at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-doublebackslashedparen-atpt 'ar-doublebackslashedparen-doublebackslash-atpt)
(defun ar-doublebackslashedparen-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-doublebackslashedparen-atpt 'ar-doublebackslashedparen-doubleslash-atpt)
(defun ar-doublebackslashedparen-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-doublebackslashedparen-atpt 'ar-doublebackslashedparen-doublebackslashparen-atpt)
(defun ar-doublebackslashedparen-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-doublebackslashedparen-atpt 'ar-doublebackslashedparen-slashparen-atpt)
(defun ar-doublebackslashedparen-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-comment-doublebackslashedparen-atpt (&optional no-delimiters)
  "Comments DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-comment 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put a comma after DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-commatize 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-quote-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-quote 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-doublebackslashedparen-atpt 'ar-doublebackslashedparen-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-doublebackslashedparen-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around DOUBLEBACKSLASHEDPAREN at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-doublebackslashedparen-atpt 'ar-doublebackslashedparen-mark-atpt)
(defun ar-doublebackslashedparen-mark-atpt ()
  "Marks DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive)
  (ar-th-mark 'doublebackslashedparen))

(defalias 'ar-hide-doublebackslashedparen-atpt 'ar-doublebackslashedparen-hide-atpt)
(defun ar-doublebackslashedparen-hide-atpt ()
  "Hides DOUBLEBACKSLASHEDPAREN at point. "
  (interactive)
  (ar-th-hide 'doublebackslashedparen))

(defalias 'ar-show-doublebackslashedparen-atpt 'ar-doublebackslashedparen-show-atpt)
(defun ar-doublebackslashedparen-show-atpt ()
  "Shows hidden DOUBLEBACKSLASHEDPAREN at point. "
  (interactive)
  (ar-th-show 'doublebackslashedparen))

(defalias 'ar-hide-show-doublebackslashedparen-atpt 'ar-doublebackslashedparen-hide-show-atpt)
(defun ar-doublebackslashedparen-hide-show-atpt ()
  "Alternatively hides or shows DOUBLEBACKSLASHEDPAREN at point. "
  (interactive)
  (ar-th-hide-show 'doublebackslashedparen))

(defalias 'ar-highlight-doublebackslashedparen-atpt-mode 'ar-doublebackslashedparen-highlight-atpt-mode)

(defun ar-doublebackslashedparen-highlight-atpt-mode (&optional no-delimiters)
  "Toggles doublebackslashedparen-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-doublebackslashedparen-atpt 'ar-doublebackslashedparen-kill-atpt)
(defun ar-doublebackslashedparen-kill-atpt (&optional no-delimiters)
  "Kills DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-kill 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-doublebackslashedparen-atpt 'ar-doublebackslashedparen-kill-backward-atpt)
(defun ar-doublebackslashedparen-kill-backward-atpt (&optional no-delimiters)
  "Kills DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-doublebackslashedparen-atpt 'ar-doublebackslashedparen-leftrightsinglequote-atpt)
(defun ar-doublebackslashedparen-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-doublebackslashedparen-atpt 'ar-doublebackslashedparen-parentize-atpt)
;; ;;;###autoload
;; (defun ar-doublebackslashedparen-parentize-atpt (&optional no-delimiters)
;;   "Parentizes DOUBLEBACKSLASHEDPAREN at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-doublebackslashedparen-atpt 'ar-doublebackslashedparen-separate-atpt)
(defun ar-doublebackslashedparen-separate-atpt (&optional no-delimiters)
  "Separates DOUBLEBACKSLASHEDPAREN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-doublebackslashedparen-atpt 'ar-doublebackslashedparen-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-doublebackslashedparen-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes DOUBLEBACKSLASHEDPAREN at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-doublebackslashedparen-atpt 'ar-doublebackslashedparen-triplequotedq-atpt)
(defun ar-doublebackslashedparen-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doublebackslashedparen. "
  (interactive "*p")
  (ar-th-triplequotedq 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-doublebackslashedparen-atpt 'ar-doublebackslashedparen-triplequotesq-atpt)
(defun ar-doublebackslashedparen-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doublebackslashedparen. "
  (interactive "*p")
  (ar-th-triplequotesq 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashedparen-trim-atpt 'ar-trim-doublebackslashedparen-atpt)
(defun ar-trim-doublebackslashedparen-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashedparen t t))

(defalias 'ar-trim-left-doublebackslashedparen-atpt 'ar-trim-doublebackslashedparen-left-atpt)
(defun ar-trim-doublebackslashedparen-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashedparen t nil))

(defalias 'ar-trim-right-doublebackslashedparen-atpt 'ar-trim-doublebackslashedparen-right-atpt)
(defun ar-trim-doublebackslashedparen-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashedparen nil t))

(defun ar-underscore-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLEBACKSLASHEDPAREN. "
  (interactive "*p")
  (ar-th-underscore 'doublebackslashedparen no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-doublebackslashedparen-whitespace-atpt 'ar-whitespace-doublebackslashedparen-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-doublebackslashedparen-atpt (&optional no-delimiters)
;;   "Put whitespace char around DOUBLEBACKSLASHEDPAREN. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'doublebackslashedparen nil t))

(defalias 'ar-forward-doublebackslashedparen-atpt 'ar-doublebackslashedparen-forward-atpt)
(defun ar-doublebackslashedparen-forward-atpt (&optional arg)
  "Moves forward over DOUBLEBACKSLASHEDPAREN at point if any, does nothing otherwise.
Returns end position of DOUBLEBACKSLASHEDPAREN "
  (interactive "p")
  (ar-th-forward 'doublebackslashedparen arg (called-interactively-p 'any)))

(defalias 'ar-backward-doublebackslashedparen-atpt 'ar-doublebackslashedparen-backward-atpt)
(defun ar-doublebackslashedparen-backward-atpt (&optional arg)
  "Moves backward over DOUBLEBACKSLASHEDPAREN before point if any, does nothing otherwise.
Returns beginning position of DOUBLEBACKSLASHEDPAREN "
  (interactive "p")
  (ar-th-backward 'doublebackslashedparen arg (called-interactively-p 'any)))

(defalias 'ar-transpose-doublebackslashedparen-atpt 'ar-doublebackslashedparen-transpose-atpt)
(defun ar-doublebackslashedparen-transpose-atpt (&optional arg)
  "Transposes DOUBLEBACKSLASHEDPAREN with DOUBLEBACKSLASHEDPAREN before point if any. "
  (interactive "*p")
  (ar-th-transpose 'doublebackslashedparen arg (called-interactively-p 'any)))

(defalias 'ar-sort-doublebackslashedparen-atpt 'ar-doublebackslashedparen-sort-atpt)
(defun ar-doublebackslashedparen-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts doublebackslashedparens in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'doublebackslashedparen reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-doublebackslashedparen-atpt 'ar-doublebackslashedparen-check-atpt)
(defun ar-doublebackslashedparen-check-atpt ()
  "Return t if a DOUBLEBACKSLASHEDPAREN at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-doublebackslashedparen-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doublebackslashedparen-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-doubleslashed-atpt (&optional arg no-delimiters)
  "Returns doubleslashed at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'doubleslashed arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-doubleslashed-atpt 'ar-doubleslashed-bounds-atpt)
(defun ar-doubleslashed-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of doubleslashed if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslashed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLESLASHED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslashed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLESLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-doubleslashed-atpt 'ar-doubleslashed-beginning-atpt)
(defun ar-doubleslashed-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DOUBLESLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-doubleslashed-atpt 'ar-doubleslashed-end-atpt)
(defun ar-doubleslashed-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DOUBLESLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-doubleslashed-p-atpt 'ar-doubleslashed-in-p-atpt)
(defun ar-doubleslashed-in-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLESLASHED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-doubleslashed-atpt 'ar-doubleslashed-length-atpt)
(defun ar-doubleslashed-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DOUBLESLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-doubleslashed-atpt 'ar-doubleslashed-copy-atpt)
(defun ar-doubleslashed-copy-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLESLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-doubleslashed-atpt 'ar-doubleslashed-delete-atpt)
(defun ar-doubleslashed-delete-atpt (&optional arg)
  "Deletes DOUBLESLASHED at point if any. "
  (interactive "*p")
  (ar-th-delete 'doubleslashed arg (called-interactively-p 'any)))

(defalias 'ar-delete-doubleslashed-in-region 'ar-doubleslashed-delete-in-region)
(defun ar-doubleslashed-delete-in-region (beg end)
  "Deletes DOUBLESLASHED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doubleslashed beg end (called-interactively-p 'any)))

(defun ar-blok-doubleslashed-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around doubleslashed.
  Returns blok or nil if no DOUBLESLASHED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-doubleslashed-atpt 'ar-doubleslashed-backslashparen-atpt)
(defun ar-backslashparen-doubleslashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doubleslashed at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-doubleslashed-atpt 'ar-doubleslashed-doublebackslash-atpt)
(defun ar-doubleslashed-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLESLASHED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-doubleslashed-atpt 'ar-doubleslashed-doubleslash-atpt)
(defun ar-doubleslashed-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLESLASHED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-doubleslashed-atpt 'ar-doubleslashed-doublebackslashparen-atpt)
(defun ar-doubleslashed-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLESLASHED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-doubleslashed-atpt 'ar-doubleslashed-slashparen-atpt)
(defun ar-doubleslashed-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLESLASHED at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defun ar-comment-doubleslashed-atpt (&optional no-delimiters)
  "Comments DOUBLESLASHED at point if any. "
  (interactive "*p")
  (ar-th-comment 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-doubleslashed-atpt (&optional no-delimiters)
  "Put a comma after DOUBLESLASHED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defun ar-quote-doubleslashed-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLESLASHED at point if any. "
  (interactive "*p")
  (ar-th-quote 'doubleslashed no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-doubleslashed-atpt 'ar-doubleslashed-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-doubleslashed-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around DOUBLESLASHED at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-doubleslashed-atpt 'ar-doubleslashed-mark-atpt)
(defun ar-doubleslashed-mark-atpt ()
  "Marks DOUBLESLASHED at point if any. "
  (interactive)
  (ar-th-mark 'doubleslashed))

(defalias 'ar-hide-doubleslashed-atpt 'ar-doubleslashed-hide-atpt)
(defun ar-doubleslashed-hide-atpt ()
  "Hides DOUBLESLASHED at point. "
  (interactive)
  (ar-th-hide 'doubleslashed))

(defalias 'ar-show-doubleslashed-atpt 'ar-doubleslashed-show-atpt)
(defun ar-doubleslashed-show-atpt ()
  "Shows hidden DOUBLESLASHED at point. "
  (interactive)
  (ar-th-show 'doubleslashed))

(defalias 'ar-hide-show-doubleslashed-atpt 'ar-doubleslashed-hide-show-atpt)
(defun ar-doubleslashed-hide-show-atpt ()
  "Alternatively hides or shows DOUBLESLASHED at point. "
  (interactive)
  (ar-th-hide-show 'doubleslashed))

(defalias 'ar-highlight-doubleslashed-atpt-mode 'ar-doubleslashed-highlight-atpt-mode)

(defun ar-doubleslashed-highlight-atpt-mode (&optional no-delimiters)
  "Toggles doubleslashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-doubleslashed-atpt 'ar-doubleslashed-kill-atpt)
(defun ar-doubleslashed-kill-atpt (&optional no-delimiters)
  "Kills DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-kill 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-doubleslashed-atpt 'ar-doubleslashed-kill-backward-atpt)
(defun ar-doubleslashed-kill-backward-atpt (&optional no-delimiters)
  "Kills DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-doubleslashed-atpt 'ar-doubleslashed-leftrightsinglequote-atpt)
(defun ar-doubleslashed-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'doubleslashed no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-doubleslashed-atpt 'ar-doubleslashed-parentize-atpt)
;; ;;;###autoload
;; (defun ar-doubleslashed-parentize-atpt (&optional no-delimiters)
;;   "Parentizes DOUBLESLASHED at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-doubleslashed-atpt 'ar-doubleslashed-separate-atpt)
(defun ar-doubleslashed-separate-atpt (&optional no-delimiters)
  "Separates DOUBLESLASHED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'doubleslashed no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-doubleslashed-atpt 'ar-doubleslashed-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-doubleslashed-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes DOUBLESLASHED at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-doubleslashed-atpt 'ar-doubleslashed-triplequotedq-atpt)
(defun ar-doubleslashed-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doubleslashed. "
  (interactive "*p")
  (ar-th-triplequotedq 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-doubleslashed-atpt 'ar-doubleslashed-triplequotesq-atpt)
(defun ar-doubleslashed-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doubleslashed. "
  (interactive "*p")
  (ar-th-triplequotesq 'doubleslashed no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslashed-trim-atpt 'ar-trim-doubleslashed-atpt)
(defun ar-trim-doubleslashed-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doubleslashed t t))

(defalias 'ar-trim-left-doubleslashed-atpt 'ar-trim-doubleslashed-left-atpt)
(defun ar-trim-doubleslashed-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doubleslashed t nil))

(defalias 'ar-trim-right-doubleslashed-atpt 'ar-trim-doubleslashed-right-atpt)
(defun ar-trim-doubleslashed-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doubleslashed nil t))

(defun ar-underscore-doubleslashed-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLESLASHED. "
  (interactive "*p")
  (ar-th-underscore 'doubleslashed no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-doubleslashed-whitespace-atpt 'ar-whitespace-doubleslashed-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-doubleslashed-atpt (&optional no-delimiters)
;;   "Put whitespace char around DOUBLESLASHED. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'doubleslashed nil t))

(defalias 'ar-forward-doubleslashed-atpt 'ar-doubleslashed-forward-atpt)
(defun ar-doubleslashed-forward-atpt (&optional arg)
  "Moves forward over DOUBLESLASHED at point if any, does nothing otherwise.
Returns end position of DOUBLESLASHED "
  (interactive "p")
  (ar-th-forward 'doubleslashed arg (called-interactively-p 'any)))

(defalias 'ar-backward-doubleslashed-atpt 'ar-doubleslashed-backward-atpt)
(defun ar-doubleslashed-backward-atpt (&optional arg)
  "Moves backward over DOUBLESLASHED before point if any, does nothing otherwise.
Returns beginning position of DOUBLESLASHED "
  (interactive "p")
  (ar-th-backward 'doubleslashed arg (called-interactively-p 'any)))

(defalias 'ar-transpose-doubleslashed-atpt 'ar-doubleslashed-transpose-atpt)
(defun ar-doubleslashed-transpose-atpt (&optional arg)
  "Transposes DOUBLESLASHED with DOUBLESLASHED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'doubleslashed arg (called-interactively-p 'any)))

(defalias 'ar-sort-doubleslashed-atpt 'ar-doubleslashed-sort-atpt)
(defun ar-doubleslashed-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts doubleslasheds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'doubleslashed reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-doubleslashed-atpt 'ar-doubleslashed-check-atpt)
(defun ar-doubleslashed-check-atpt ()
  "Return t if a DOUBLESLASHED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-doubleslashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doubleslashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-doubleslashedparen-atpt (&optional arg no-delimiters)
  "Returns doubleslashedparen at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'doubleslashedparen arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-doubleslashedparen-atpt 'ar-doubleslashedparen-bounds-atpt)
(defun ar-doubleslashedparen-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of doubleslashedparen if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslashedparen-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLESLASHEDPAREN at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-doubleslashedparen-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLESLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-doubleslashedparen-atpt 'ar-doubleslashedparen-beginning-atpt)
(defun ar-doubleslashedparen-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DOUBLESLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-doubleslashedparen-atpt 'ar-doubleslashedparen-end-atpt)
(defun ar-doubleslashedparen-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DOUBLESLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-doubleslashedparen-p-atpt 'ar-doubleslashedparen-in-p-atpt)
(defun ar-doubleslashedparen-in-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLESLASHEDPAREN at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-doubleslashedparen-atpt 'ar-doubleslashedparen-length-atpt)
(defun ar-doubleslashedparen-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DOUBLESLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-doubleslashedparen-atpt 'ar-doubleslashedparen-copy-atpt)
(defun ar-doubleslashedparen-copy-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLESLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-doubleslashedparen-atpt 'ar-doubleslashedparen-delete-atpt)
(defun ar-doubleslashedparen-delete-atpt (&optional arg)
  "Deletes DOUBLESLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-delete 'doubleslashedparen arg (called-interactively-p 'any)))

(defalias 'ar-delete-doubleslashedparen-in-region 'ar-doubleslashedparen-delete-in-region)
(defun ar-doubleslashedparen-delete-in-region (beg end)
  "Deletes DOUBLESLASHEDPAREN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doubleslashedparen beg end (called-interactively-p 'any)))

(defun ar-blok-doubleslashedparen-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around doubleslashedparen.
  Returns blok or nil if no DOUBLESLASHEDPAREN at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-doubleslashedparen-atpt 'ar-doubleslashedparen-backslashparen-atpt)
(defun ar-backslashparen-doubleslashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doubleslashedparen at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-doubleslashedparen-atpt 'ar-doubleslashedparen-doublebackslash-atpt)
(defun ar-doubleslashedparen-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-doubleslashedparen-atpt 'ar-doubleslashedparen-doubleslash-atpt)
(defun ar-doubleslashedparen-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-doubleslashedparen-atpt 'ar-doubleslashedparen-doublebackslashparen-atpt)
(defun ar-doubleslashedparen-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-doubleslashedparen-atpt 'ar-doubleslashedparen-slashparen-atpt)
(defun ar-doubleslashedparen-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-comment-doubleslashedparen-atpt (&optional no-delimiters)
  "Comments DOUBLESLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-comment 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-doubleslashedparen-atpt (&optional no-delimiters)
  "Put a comma after DOUBLESLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-commatize 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-quote-doubleslashedparen-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLESLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-quote 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-doubleslashedparen-atpt 'ar-doubleslashedparen-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-doubleslashedparen-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around DOUBLESLASHEDPAREN at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-doubleslashedparen-atpt 'ar-doubleslashedparen-mark-atpt)
(defun ar-doubleslashedparen-mark-atpt ()
  "Marks DOUBLESLASHEDPAREN at point if any. "
  (interactive)
  (ar-th-mark 'doubleslashedparen))

(defalias 'ar-hide-doubleslashedparen-atpt 'ar-doubleslashedparen-hide-atpt)
(defun ar-doubleslashedparen-hide-atpt ()
  "Hides DOUBLESLASHEDPAREN at point. "
  (interactive)
  (ar-th-hide 'doubleslashedparen))

(defalias 'ar-show-doubleslashedparen-atpt 'ar-doubleslashedparen-show-atpt)
(defun ar-doubleslashedparen-show-atpt ()
  "Shows hidden DOUBLESLASHEDPAREN at point. "
  (interactive)
  (ar-th-show 'doubleslashedparen))

(defalias 'ar-hide-show-doubleslashedparen-atpt 'ar-doubleslashedparen-hide-show-atpt)
(defun ar-doubleslashedparen-hide-show-atpt ()
  "Alternatively hides or shows DOUBLESLASHEDPAREN at point. "
  (interactive)
  (ar-th-hide-show 'doubleslashedparen))

(defalias 'ar-highlight-doubleslashedparen-atpt-mode 'ar-doubleslashedparen-highlight-atpt-mode)

(defun ar-doubleslashedparen-highlight-atpt-mode (&optional no-delimiters)
  "Toggles doubleslashedparen-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-doubleslashedparen-atpt 'ar-doubleslashedparen-kill-atpt)
(defun ar-doubleslashedparen-kill-atpt (&optional no-delimiters)
  "Kills DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-kill 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-doubleslashedparen-atpt 'ar-doubleslashedparen-kill-backward-atpt)
(defun ar-doubleslashedparen-kill-backward-atpt (&optional no-delimiters)
  "Kills DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-doubleslashedparen-atpt 'ar-doubleslashedparen-leftrightsinglequote-atpt)
(defun ar-doubleslashedparen-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-doubleslashedparen-atpt 'ar-doubleslashedparen-parentize-atpt)
;; ;;;###autoload
;; (defun ar-doubleslashedparen-parentize-atpt (&optional no-delimiters)
;;   "Parentizes DOUBLESLASHEDPAREN at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-doubleslashedparen-atpt 'ar-doubleslashedparen-separate-atpt)
(defun ar-doubleslashedparen-separate-atpt (&optional no-delimiters)
  "Separates DOUBLESLASHEDPAREN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-doubleslashedparen-atpt 'ar-doubleslashedparen-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-doubleslashedparen-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes DOUBLESLASHEDPAREN at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-doubleslashedparen-atpt 'ar-doubleslashedparen-triplequotedq-atpt)
(defun ar-doubleslashedparen-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doubleslashedparen. "
  (interactive "*p")
  (ar-th-triplequotedq 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-doubleslashedparen-atpt 'ar-doubleslashedparen-triplequotesq-atpt)
(defun ar-doubleslashedparen-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doubleslashedparen. "
  (interactive "*p")
  (ar-th-triplequotesq 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslashedparen-trim-atpt 'ar-trim-doubleslashedparen-atpt)
(defun ar-trim-doubleslashedparen-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doubleslashedparen t t))

(defalias 'ar-trim-left-doubleslashedparen-atpt 'ar-trim-doubleslashedparen-left-atpt)
(defun ar-trim-doubleslashedparen-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doubleslashedparen t nil))

(defalias 'ar-trim-right-doubleslashedparen-atpt 'ar-trim-doubleslashedparen-right-atpt)
(defun ar-trim-doubleslashedparen-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doubleslashedparen nil t))

(defun ar-underscore-doubleslashedparen-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLESLASHEDPAREN. "
  (interactive "*p")
  (ar-th-underscore 'doubleslashedparen no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-doubleslashedparen-whitespace-atpt 'ar-whitespace-doubleslashedparen-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-doubleslashedparen-atpt (&optional no-delimiters)
;;   "Put whitespace char around DOUBLESLASHEDPAREN. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'doubleslashedparen nil t))

(defalias 'ar-forward-doubleslashedparen-atpt 'ar-doubleslashedparen-forward-atpt)
(defun ar-doubleslashedparen-forward-atpt (&optional arg)
  "Moves forward over DOUBLESLASHEDPAREN at point if any, does nothing otherwise.
Returns end position of DOUBLESLASHEDPAREN "
  (interactive "p")
  (ar-th-forward 'doubleslashedparen arg (called-interactively-p 'any)))

(defalias 'ar-backward-doubleslashedparen-atpt 'ar-doubleslashedparen-backward-atpt)
(defun ar-doubleslashedparen-backward-atpt (&optional arg)
  "Moves backward over DOUBLESLASHEDPAREN before point if any, does nothing otherwise.
Returns beginning position of DOUBLESLASHEDPAREN "
  (interactive "p")
  (ar-th-backward 'doubleslashedparen arg (called-interactively-p 'any)))

(defalias 'ar-transpose-doubleslashedparen-atpt 'ar-doubleslashedparen-transpose-atpt)
(defun ar-doubleslashedparen-transpose-atpt (&optional arg)
  "Transposes DOUBLESLASHEDPAREN with DOUBLESLASHEDPAREN before point if any. "
  (interactive "*p")
  (ar-th-transpose 'doubleslashedparen arg (called-interactively-p 'any)))

(defalias 'ar-sort-doubleslashedparen-atpt 'ar-doubleslashedparen-sort-atpt)
(defun ar-doubleslashedparen-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts doubleslashedparens in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'doubleslashedparen reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-doubleslashedparen-atpt 'ar-doubleslashedparen-check-atpt)
(defun ar-doubleslashedparen-check-atpt ()
  "Return t if a DOUBLESLASHEDPAREN at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-doubleslashedparen-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doubleslashedparen-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-markup-atpt (&optional arg no-delimiters)
  "Returns markup at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'markup arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-markup-atpt 'ar-markup-bounds-atpt)
(defun ar-markup-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of markup if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'markup no-delimiters (called-interactively-p 'any)))

(defun ar-markup-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position MARKUP at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'markup no-delimiters (called-interactively-p 'any)))

(defun ar-markup-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of MARKUP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-markup-atpt 'ar-markup-beginning-atpt)
(defun ar-markup-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class MARKUP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-markup-atpt 'ar-markup-end-atpt)
(defun ar-markup-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class MARKUP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-markup-p-atpt 'ar-markup-in-p-atpt)
(defun ar-markup-in-p-atpt (&optional no-delimiters)
  "Returns bounds of MARKUP at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-markup-atpt 'ar-markup-length-atpt)
(defun ar-markup-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class MARKUP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-markup-atpt 'ar-markup-copy-atpt)
(defun ar-markup-copy-atpt (&optional no-delimiters)
  "Returns a copy of MARKUP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-markup-atpt 'ar-markup-delete-atpt)
(defun ar-markup-delete-atpt (&optional arg)
  "Deletes MARKUP at point if any. "
  (interactive "*p")
  (ar-th-delete 'markup arg (called-interactively-p 'any)))

(defalias 'ar-delete-markup-in-region 'ar-markup-delete-in-region)
(defun ar-markup-delete-in-region (beg end)
  "Deletes MARKUP at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'markup beg end (called-interactively-p 'any)))

(defun ar-blok-markup-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around markup.
  Returns blok or nil if no MARKUP at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-markup-atpt 'ar-markup-backslashparen-atpt)
(defun ar-backslashparen-markup-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around markup at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-markup-atpt 'ar-markup-doublebackslash-atpt)
(defun ar-markup-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around MARKUP at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-markup-atpt 'ar-markup-doubleslash-atpt)
(defun ar-markup-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around MARKUP at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-markup-atpt 'ar-markup-doublebackslashparen-atpt)
(defun ar-markup-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around MARKUP at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-markup-atpt 'ar-markup-slashparen-atpt)
(defun ar-markup-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MARKUP at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'markup no-delimiters (called-interactively-p 'any)))

(defun ar-comment-markup-atpt (&optional no-delimiters)
  "Comments MARKUP at point if any. "
  (interactive "*p")
  (ar-th-comment 'markup no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-markup-atpt (&optional no-delimiters)
  "Put a comma after MARKUP at point if any. "
  (interactive "*p")
  (ar-th-commatize 'markup no-delimiters (called-interactively-p 'any)))

(defun ar-quote-markup-atpt (&optional no-delimiters)
  "Put a singlequote before MARKUP at point if any. "
  (interactive "*p")
  (ar-th-quote 'markup no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-markup-atpt 'ar-markup-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-markup-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around MARKUP at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-markup-atpt 'ar-markup-mark-atpt)
(defun ar-markup-mark-atpt ()
  "Marks MARKUP at point if any. "
  (interactive)
  (ar-th-mark 'markup))

(defalias 'ar-hide-markup-atpt 'ar-markup-hide-atpt)
(defun ar-markup-hide-atpt ()
  "Hides MARKUP at point. "
  (interactive)
  (ar-th-hide 'markup))

(defalias 'ar-show-markup-atpt 'ar-markup-show-atpt)
(defun ar-markup-show-atpt ()
  "Shows hidden MARKUP at point. "
  (interactive)
  (ar-th-show 'markup))

(defalias 'ar-hide-show-markup-atpt 'ar-markup-hide-show-atpt)
(defun ar-markup-hide-show-atpt ()
  "Alternatively hides or shows MARKUP at point. "
  (interactive)
  (ar-th-hide-show 'markup))

(defalias 'ar-highlight-markup-atpt-mode 'ar-markup-highlight-atpt-mode)

(defun ar-markup-highlight-atpt-mode (&optional no-delimiters)
  "Toggles markup-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-markup-atpt 'ar-markup-kill-atpt)
(defun ar-markup-kill-atpt (&optional no-delimiters)
  "Kills MARKUP at point if any. "
  (interactive "*P")
  (ar-th-kill 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-markup-atpt 'ar-markup-kill-backward-atpt)
(defun ar-markup-kill-backward-atpt (&optional no-delimiters)
  "Kills MARKUP at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-markup-atpt 'ar-markup-leftrightsinglequote-atpt)
(defun ar-markup-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'markup no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-markup-atpt 'ar-markup-parentize-atpt)
;; ;;;###autoload
;; (defun ar-markup-parentize-atpt (&optional no-delimiters)
;;   "Parentizes MARKUP at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-markup-atpt 'ar-markup-separate-atpt)
(defun ar-markup-separate-atpt (&optional no-delimiters)
  "Separates MARKUP at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'markup no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-markup-atpt 'ar-markup-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-markup-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes MARKUP at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-markup-atpt 'ar-markup-triplequotedq-atpt)
(defun ar-markup-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around markup. "
  (interactive "*p")
  (ar-th-triplequotedq 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-markup-atpt 'ar-markup-triplequotesq-atpt)
(defun ar-markup-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around markup. "
  (interactive "*p")
  (ar-th-triplequotesq 'markup no-delimiters (called-interactively-p 'any)))

(defalias 'ar-markup-trim-atpt 'ar-trim-markup-atpt)
(defun ar-trim-markup-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'markup t t))

(defalias 'ar-trim-left-markup-atpt 'ar-trim-markup-left-atpt)
(defun ar-trim-markup-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'markup t nil))

(defalias 'ar-trim-right-markup-atpt 'ar-trim-markup-right-atpt)
(defun ar-trim-markup-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'markup nil t))

(defun ar-underscore-markup-atpt (&optional no-delimiters)
  "Put underscore char around MARKUP. "
  (interactive "*p")
  (ar-th-underscore 'markup no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-markup-whitespace-atpt 'ar-whitespace-markup-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-markup-atpt (&optional no-delimiters)
;;   "Put whitespace char around MARKUP. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'markup nil t))

(defalias 'ar-forward-markup-atpt 'ar-markup-forward-atpt)
(defun ar-markup-forward-atpt (&optional arg)
  "Moves forward over MARKUP at point if any, does nothing otherwise.
Returns end position of MARKUP "
  (interactive "p")
  (ar-th-forward 'markup arg (called-interactively-p 'any)))

(defalias 'ar-backward-markup-atpt 'ar-markup-backward-atpt)
(defun ar-markup-backward-atpt (&optional arg)
  "Moves backward over MARKUP before point if any, does nothing otherwise.
Returns beginning position of MARKUP "
  (interactive "p")
  (ar-th-backward 'markup arg (called-interactively-p 'any)))

(defalias 'ar-transpose-markup-atpt 'ar-markup-transpose-atpt)
(defun ar-markup-transpose-atpt (&optional arg)
  "Transposes MARKUP with MARKUP before point if any. "
  (interactive "*p")
  (ar-th-transpose 'markup arg (called-interactively-p 'any)))

(defalias 'ar-sort-markup-atpt 'ar-markup-sort-atpt)
(defun ar-markup-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts markups in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'markup reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-markup-atpt 'ar-markup-check-atpt)
(defun ar-markup-check-atpt ()
  "Return t if a MARKUP at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-markup-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-markup-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-mldata-atpt (&optional arg no-delimiters)
  "Returns mldata at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'mldata arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-mldata-atpt 'ar-mldata-bounds-atpt)
(defun ar-mldata-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of mldata if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'mldata no-delimiters (called-interactively-p 'any)))

(defun ar-mldata-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position MLDATA at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'mldata no-delimiters (called-interactively-p 'any)))

(defun ar-mldata-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of MLDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-mldata-atpt 'ar-mldata-beginning-atpt)
(defun ar-mldata-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class MLDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-mldata-atpt 'ar-mldata-end-atpt)
(defun ar-mldata-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class MLDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-mldata-p-atpt 'ar-mldata-in-p-atpt)
(defun ar-mldata-in-p-atpt (&optional no-delimiters)
  "Returns bounds of MLDATA at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-mldata-atpt 'ar-mldata-length-atpt)
(defun ar-mldata-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class MLDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-mldata-atpt 'ar-mldata-copy-atpt)
(defun ar-mldata-copy-atpt (&optional no-delimiters)
  "Returns a copy of MLDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-mldata-atpt 'ar-mldata-delete-atpt)
(defun ar-mldata-delete-atpt (&optional arg)
  "Deletes MLDATA at point if any. "
  (interactive "*p")
  (ar-th-delete 'mldata arg (called-interactively-p 'any)))

(defalias 'ar-delete-mldata-in-region 'ar-mldata-delete-in-region)
(defun ar-mldata-delete-in-region (beg end)
  "Deletes MLDATA at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'mldata beg end (called-interactively-p 'any)))

(defun ar-blok-mldata-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around mldata.
  Returns blok or nil if no MLDATA at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-mldata-atpt 'ar-mldata-backslashparen-atpt)
(defun ar-backslashparen-mldata-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around mldata at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-mldata-atpt 'ar-mldata-doublebackslash-atpt)
(defun ar-mldata-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around MLDATA at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-mldata-atpt 'ar-mldata-doubleslash-atpt)
(defun ar-mldata-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around MLDATA at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-mldata-atpt 'ar-mldata-doublebackslashparen-atpt)
(defun ar-mldata-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around MLDATA at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-mldata-atpt 'ar-mldata-slashparen-atpt)
(defun ar-mldata-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MLDATA at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'mldata no-delimiters (called-interactively-p 'any)))

(defun ar-comment-mldata-atpt (&optional no-delimiters)
  "Comments MLDATA at point if any. "
  (interactive "*p")
  (ar-th-comment 'mldata no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-mldata-atpt (&optional no-delimiters)
  "Put a comma after MLDATA at point if any. "
  (interactive "*p")
  (ar-th-commatize 'mldata no-delimiters (called-interactively-p 'any)))

(defun ar-quote-mldata-atpt (&optional no-delimiters)
  "Put a singlequote before MLDATA at point if any. "
  (interactive "*p")
  (ar-th-quote 'mldata no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-mldata-atpt 'ar-mldata-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-mldata-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around MLDATA at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-mldata-atpt 'ar-mldata-mark-atpt)
(defun ar-mldata-mark-atpt ()
  "Marks MLDATA at point if any. "
  (interactive)
  (ar-th-mark 'mldata))

(defalias 'ar-hide-mldata-atpt 'ar-mldata-hide-atpt)
(defun ar-mldata-hide-atpt ()
  "Hides MLDATA at point. "
  (interactive)
  (ar-th-hide 'mldata))

(defalias 'ar-show-mldata-atpt 'ar-mldata-show-atpt)
(defun ar-mldata-show-atpt ()
  "Shows hidden MLDATA at point. "
  (interactive)
  (ar-th-show 'mldata))

(defalias 'ar-hide-show-mldata-atpt 'ar-mldata-hide-show-atpt)
(defun ar-mldata-hide-show-atpt ()
  "Alternatively hides or shows MLDATA at point. "
  (interactive)
  (ar-th-hide-show 'mldata))

(defalias 'ar-highlight-mldata-atpt-mode 'ar-mldata-highlight-atpt-mode)

(defun ar-mldata-highlight-atpt-mode (&optional no-delimiters)
  "Toggles mldata-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-mldata-atpt 'ar-mldata-kill-atpt)
(defun ar-mldata-kill-atpt (&optional no-delimiters)
  "Kills MLDATA at point if any. "
  (interactive "*P")
  (ar-th-kill 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-mldata-atpt 'ar-mldata-kill-backward-atpt)
(defun ar-mldata-kill-backward-atpt (&optional no-delimiters)
  "Kills MLDATA at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-mldata-atpt 'ar-mldata-leftrightsinglequote-atpt)
(defun ar-mldata-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'mldata no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-mldata-atpt 'ar-mldata-parentize-atpt)
;; ;;;###autoload
;; (defun ar-mldata-parentize-atpt (&optional no-delimiters)
;;   "Parentizes MLDATA at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-mldata-atpt 'ar-mldata-separate-atpt)
(defun ar-mldata-separate-atpt (&optional no-delimiters)
  "Separates MLDATA at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'mldata no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-mldata-atpt 'ar-mldata-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-mldata-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes MLDATA at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-mldata-atpt 'ar-mldata-triplequotedq-atpt)
(defun ar-mldata-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around mldata. "
  (interactive "*p")
  (ar-th-triplequotedq 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-mldata-atpt 'ar-mldata-triplequotesq-atpt)
(defun ar-mldata-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around mldata. "
  (interactive "*p")
  (ar-th-triplequotesq 'mldata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mldata-trim-atpt 'ar-trim-mldata-atpt)
(defun ar-trim-mldata-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'mldata t t))

(defalias 'ar-trim-left-mldata-atpt 'ar-trim-mldata-left-atpt)
(defun ar-trim-mldata-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'mldata t nil))

(defalias 'ar-trim-right-mldata-atpt 'ar-trim-mldata-right-atpt)
(defun ar-trim-mldata-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'mldata nil t))

(defun ar-underscore-mldata-atpt (&optional no-delimiters)
  "Put underscore char around MLDATA. "
  (interactive "*p")
  (ar-th-underscore 'mldata no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-mldata-whitespace-atpt 'ar-whitespace-mldata-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-mldata-atpt (&optional no-delimiters)
;;   "Put whitespace char around MLDATA. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'mldata nil t))

(defalias 'ar-forward-mldata-atpt 'ar-mldata-forward-atpt)
(defun ar-mldata-forward-atpt (&optional arg)
  "Moves forward over MLDATA at point if any, does nothing otherwise.
Returns end position of MLDATA "
  (interactive "p")
  (ar-th-forward 'mldata arg (called-interactively-p 'any)))

(defalias 'ar-backward-mldata-atpt 'ar-mldata-backward-atpt)
(defun ar-mldata-backward-atpt (&optional arg)
  "Moves backward over MLDATA before point if any, does nothing otherwise.
Returns beginning position of MLDATA "
  (interactive "p")
  (ar-th-backward 'mldata arg (called-interactively-p 'any)))

(defalias 'ar-transpose-mldata-atpt 'ar-mldata-transpose-atpt)
(defun ar-mldata-transpose-atpt (&optional arg)
  "Transposes MLDATA with MLDATA before point if any. "
  (interactive "*p")
  (ar-th-transpose 'mldata arg (called-interactively-p 'any)))

(defalias 'ar-sort-mldata-atpt 'ar-mldata-sort-atpt)
(defun ar-mldata-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts mldatas in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'mldata reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-mldata-atpt 'ar-mldata-check-atpt)
(defun ar-mldata-check-atpt ()
  "Return t if a MLDATA at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-mldata-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-mldata-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-mlattribut-atpt (&optional arg no-delimiters)
  "Returns mlattribut at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'mlattribut arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-mlattribut-atpt 'ar-mlattribut-bounds-atpt)
(defun ar-mlattribut-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of mlattribut if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'mlattribut no-delimiters (called-interactively-p 'any)))

(defun ar-mlattribut-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position MLATTRIBUT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'mlattribut no-delimiters (called-interactively-p 'any)))

(defun ar-mlattribut-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of MLATTRIBUT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-mlattribut-atpt 'ar-mlattribut-beginning-atpt)
(defun ar-mlattribut-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class MLATTRIBUT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-mlattribut-atpt 'ar-mlattribut-end-atpt)
(defun ar-mlattribut-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class MLATTRIBUT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-mlattribut-p-atpt 'ar-mlattribut-in-p-atpt)
(defun ar-mlattribut-in-p-atpt (&optional no-delimiters)
  "Returns bounds of MLATTRIBUT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-mlattribut-atpt 'ar-mlattribut-length-atpt)
(defun ar-mlattribut-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class MLATTRIBUT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-mlattribut-atpt 'ar-mlattribut-copy-atpt)
(defun ar-mlattribut-copy-atpt (&optional no-delimiters)
  "Returns a copy of MLATTRIBUT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-mlattribut-atpt 'ar-mlattribut-delete-atpt)
(defun ar-mlattribut-delete-atpt (&optional arg)
  "Deletes MLATTRIBUT at point if any. "
  (interactive "*p")
  (ar-th-delete 'mlattribut arg (called-interactively-p 'any)))

(defalias 'ar-delete-mlattribut-in-region 'ar-mlattribut-delete-in-region)
(defun ar-mlattribut-delete-in-region (beg end)
  "Deletes MLATTRIBUT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'mlattribut beg end (called-interactively-p 'any)))

(defun ar-blok-mlattribut-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around mlattribut.
  Returns blok or nil if no MLATTRIBUT at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-mlattribut-atpt 'ar-mlattribut-backslashparen-atpt)
(defun ar-backslashparen-mlattribut-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around mlattribut at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-mlattribut-atpt 'ar-mlattribut-doublebackslash-atpt)
(defun ar-mlattribut-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around MLATTRIBUT at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-mlattribut-atpt 'ar-mlattribut-doubleslash-atpt)
(defun ar-mlattribut-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around MLATTRIBUT at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-mlattribut-atpt 'ar-mlattribut-doublebackslashparen-atpt)
(defun ar-mlattribut-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around MLATTRIBUT at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-mlattribut-atpt 'ar-mlattribut-slashparen-atpt)
(defun ar-mlattribut-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MLATTRIBUT at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'mlattribut no-delimiters (called-interactively-p 'any)))

(defun ar-comment-mlattribut-atpt (&optional no-delimiters)
  "Comments MLATTRIBUT at point if any. "
  (interactive "*p")
  (ar-th-comment 'mlattribut no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-mlattribut-atpt (&optional no-delimiters)
  "Put a comma after MLATTRIBUT at point if any. "
  (interactive "*p")
  (ar-th-commatize 'mlattribut no-delimiters (called-interactively-p 'any)))

(defun ar-quote-mlattribut-atpt (&optional no-delimiters)
  "Put a singlequote before MLATTRIBUT at point if any. "
  (interactive "*p")
  (ar-th-quote 'mlattribut no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-mlattribut-atpt 'ar-mlattribut-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-mlattribut-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around MLATTRIBUT at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-mlattribut-atpt 'ar-mlattribut-mark-atpt)
(defun ar-mlattribut-mark-atpt ()
  "Marks MLATTRIBUT at point if any. "
  (interactive)
  (ar-th-mark 'mlattribut))

(defalias 'ar-hide-mlattribut-atpt 'ar-mlattribut-hide-atpt)
(defun ar-mlattribut-hide-atpt ()
  "Hides MLATTRIBUT at point. "
  (interactive)
  (ar-th-hide 'mlattribut))

(defalias 'ar-show-mlattribut-atpt 'ar-mlattribut-show-atpt)
(defun ar-mlattribut-show-atpt ()
  "Shows hidden MLATTRIBUT at point. "
  (interactive)
  (ar-th-show 'mlattribut))

(defalias 'ar-hide-show-mlattribut-atpt 'ar-mlattribut-hide-show-atpt)
(defun ar-mlattribut-hide-show-atpt ()
  "Alternatively hides or shows MLATTRIBUT at point. "
  (interactive)
  (ar-th-hide-show 'mlattribut))

(defalias 'ar-highlight-mlattribut-atpt-mode 'ar-mlattribut-highlight-atpt-mode)

(defun ar-mlattribut-highlight-atpt-mode (&optional no-delimiters)
  "Toggles mlattribut-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-mlattribut-atpt 'ar-mlattribut-kill-atpt)
(defun ar-mlattribut-kill-atpt (&optional no-delimiters)
  "Kills MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-kill 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-mlattribut-atpt 'ar-mlattribut-kill-backward-atpt)
(defun ar-mlattribut-kill-backward-atpt (&optional no-delimiters)
  "Kills MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-mlattribut-atpt 'ar-mlattribut-leftrightsinglequote-atpt)
(defun ar-mlattribut-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'mlattribut no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-mlattribut-atpt 'ar-mlattribut-parentize-atpt)
;; ;;;###autoload
;; (defun ar-mlattribut-parentize-atpt (&optional no-delimiters)
;;   "Parentizes MLATTRIBUT at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-mlattribut-atpt 'ar-mlattribut-separate-atpt)
(defun ar-mlattribut-separate-atpt (&optional no-delimiters)
  "Separates MLATTRIBUT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'mlattribut no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-mlattribut-atpt 'ar-mlattribut-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-mlattribut-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes MLATTRIBUT at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-mlattribut-atpt 'ar-mlattribut-triplequotedq-atpt)
(defun ar-mlattribut-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around mlattribut. "
  (interactive "*p")
  (ar-th-triplequotedq 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-mlattribut-atpt 'ar-mlattribut-triplequotesq-atpt)
(defun ar-mlattribut-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around mlattribut. "
  (interactive "*p")
  (ar-th-triplequotesq 'mlattribut no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mlattribut-trim-atpt 'ar-trim-mlattribut-atpt)
(defun ar-trim-mlattribut-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'mlattribut t t))

(defalias 'ar-trim-left-mlattribut-atpt 'ar-trim-mlattribut-left-atpt)
(defun ar-trim-mlattribut-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'mlattribut t nil))

(defalias 'ar-trim-right-mlattribut-atpt 'ar-trim-mlattribut-right-atpt)
(defun ar-trim-mlattribut-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'mlattribut nil t))

(defun ar-underscore-mlattribut-atpt (&optional no-delimiters)
  "Put underscore char around MLATTRIBUT. "
  (interactive "*p")
  (ar-th-underscore 'mlattribut no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-mlattribut-whitespace-atpt 'ar-whitespace-mlattribut-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-mlattribut-atpt (&optional no-delimiters)
;;   "Put whitespace char around MLATTRIBUT. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'mlattribut nil t))

(defalias 'ar-forward-mlattribut-atpt 'ar-mlattribut-forward-atpt)
(defun ar-mlattribut-forward-atpt (&optional arg)
  "Moves forward over MLATTRIBUT at point if any, does nothing otherwise.
Returns end position of MLATTRIBUT "
  (interactive "p")
  (ar-th-forward 'mlattribut arg (called-interactively-p 'any)))

(defalias 'ar-backward-mlattribut-atpt 'ar-mlattribut-backward-atpt)
(defun ar-mlattribut-backward-atpt (&optional arg)
  "Moves backward over MLATTRIBUT before point if any, does nothing otherwise.
Returns beginning position of MLATTRIBUT "
  (interactive "p")
  (ar-th-backward 'mlattribut arg (called-interactively-p 'any)))

(defalias 'ar-transpose-mlattribut-atpt 'ar-mlattribut-transpose-atpt)
(defun ar-mlattribut-transpose-atpt (&optional arg)
  "Transposes MLATTRIBUT with MLATTRIBUT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'mlattribut arg (called-interactively-p 'any)))

(defalias 'ar-sort-mlattribut-atpt 'ar-mlattribut-sort-atpt)
(defun ar-mlattribut-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts mlattributs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'mlattribut reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-mlattribut-atpt 'ar-mlattribut-check-atpt)
(defun ar-mlattribut-check-atpt ()
  "Return t if a MLATTRIBUT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-mlattribut-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-mlattribut-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-mltag-atpt (&optional arg no-delimiters)
  "Returns mltag at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'mltag arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-mltag-atpt 'ar-mltag-bounds-atpt)
(defun ar-mltag-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of mltag if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'mltag no-delimiters (called-interactively-p 'any)))

(defun ar-mltag-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position MLTAG at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'mltag no-delimiters (called-interactively-p 'any)))

(defun ar-mltag-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of MLTAG at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-mltag-atpt 'ar-mltag-beginning-atpt)
(defun ar-mltag-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class MLTAG at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-mltag-atpt 'ar-mltag-end-atpt)
(defun ar-mltag-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class MLTAG at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-mltag-p-atpt 'ar-mltag-in-p-atpt)
(defun ar-mltag-in-p-atpt (&optional no-delimiters)
  "Returns bounds of MLTAG at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-mltag-atpt 'ar-mltag-length-atpt)
(defun ar-mltag-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class MLTAG at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-mltag-atpt 'ar-mltag-copy-atpt)
(defun ar-mltag-copy-atpt (&optional no-delimiters)
  "Returns a copy of MLTAG at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-mltag-atpt 'ar-mltag-delete-atpt)
(defun ar-mltag-delete-atpt (&optional arg)
  "Deletes MLTAG at point if any. "
  (interactive "*p")
  (ar-th-delete 'mltag arg (called-interactively-p 'any)))

(defalias 'ar-delete-mltag-in-region 'ar-mltag-delete-in-region)
(defun ar-mltag-delete-in-region (beg end)
  "Deletes MLTAG at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'mltag beg end (called-interactively-p 'any)))

(defun ar-blok-mltag-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around mltag.
  Returns blok or nil if no MLTAG at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-mltag-atpt 'ar-mltag-backslashparen-atpt)
(defun ar-backslashparen-mltag-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around mltag at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-mltag-atpt 'ar-mltag-doublebackslash-atpt)
(defun ar-mltag-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around MLTAG at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-mltag-atpt 'ar-mltag-doubleslash-atpt)
(defun ar-mltag-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around MLTAG at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-mltag-atpt 'ar-mltag-doublebackslashparen-atpt)
(defun ar-mltag-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around MLTAG at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-mltag-atpt 'ar-mltag-slashparen-atpt)
(defun ar-mltag-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MLTAG at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'mltag no-delimiters (called-interactively-p 'any)))

(defun ar-comment-mltag-atpt (&optional no-delimiters)
  "Comments MLTAG at point if any. "
  (interactive "*p")
  (ar-th-comment 'mltag no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-mltag-atpt (&optional no-delimiters)
  "Put a comma after MLTAG at point if any. "
  (interactive "*p")
  (ar-th-commatize 'mltag no-delimiters (called-interactively-p 'any)))

(defun ar-quote-mltag-atpt (&optional no-delimiters)
  "Put a singlequote before MLTAG at point if any. "
  (interactive "*p")
  (ar-th-quote 'mltag no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-mltag-atpt 'ar-mltag-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-mltag-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around MLTAG at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-mltag-atpt 'ar-mltag-mark-atpt)
(defun ar-mltag-mark-atpt ()
  "Marks MLTAG at point if any. "
  (interactive)
  (ar-th-mark 'mltag))

(defalias 'ar-hide-mltag-atpt 'ar-mltag-hide-atpt)
(defun ar-mltag-hide-atpt ()
  "Hides MLTAG at point. "
  (interactive)
  (ar-th-hide 'mltag))

(defalias 'ar-show-mltag-atpt 'ar-mltag-show-atpt)
(defun ar-mltag-show-atpt ()
  "Shows hidden MLTAG at point. "
  (interactive)
  (ar-th-show 'mltag))

(defalias 'ar-hide-show-mltag-atpt 'ar-mltag-hide-show-atpt)
(defun ar-mltag-hide-show-atpt ()
  "Alternatively hides or shows MLTAG at point. "
  (interactive)
  (ar-th-hide-show 'mltag))

(defalias 'ar-highlight-mltag-atpt-mode 'ar-mltag-highlight-atpt-mode)

(defun ar-mltag-highlight-atpt-mode (&optional no-delimiters)
  "Toggles mltag-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-mltag-atpt 'ar-mltag-kill-atpt)
(defun ar-mltag-kill-atpt (&optional no-delimiters)
  "Kills MLTAG at point if any. "
  (interactive "*P")
  (ar-th-kill 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-mltag-atpt 'ar-mltag-kill-backward-atpt)
(defun ar-mltag-kill-backward-atpt (&optional no-delimiters)
  "Kills MLTAG at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-mltag-atpt 'ar-mltag-leftrightsinglequote-atpt)
(defun ar-mltag-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'mltag no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-mltag-atpt 'ar-mltag-parentize-atpt)
;; ;;;###autoload
;; (defun ar-mltag-parentize-atpt (&optional no-delimiters)
;;   "Parentizes MLTAG at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-mltag-atpt 'ar-mltag-separate-atpt)
(defun ar-mltag-separate-atpt (&optional no-delimiters)
  "Separates MLTAG at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'mltag no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-mltag-atpt 'ar-mltag-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-mltag-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes MLTAG at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-mltag-atpt 'ar-mltag-triplequotedq-atpt)
(defun ar-mltag-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around mltag. "
  (interactive "*p")
  (ar-th-triplequotedq 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-mltag-atpt 'ar-mltag-triplequotesq-atpt)
(defun ar-mltag-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around mltag. "
  (interactive "*p")
  (ar-th-triplequotesq 'mltag no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mltag-trim-atpt 'ar-trim-mltag-atpt)
(defun ar-trim-mltag-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'mltag t t))

(defalias 'ar-trim-left-mltag-atpt 'ar-trim-mltag-left-atpt)
(defun ar-trim-mltag-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'mltag t nil))

(defalias 'ar-trim-right-mltag-atpt 'ar-trim-mltag-right-atpt)
(defun ar-trim-mltag-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'mltag nil t))

(defun ar-underscore-mltag-atpt (&optional no-delimiters)
  "Put underscore char around MLTAG. "
  (interactive "*p")
  (ar-th-underscore 'mltag no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-mltag-whitespace-atpt 'ar-whitespace-mltag-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-mltag-atpt (&optional no-delimiters)
;;   "Put whitespace char around MLTAG. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'mltag nil t))

(defalias 'ar-forward-mltag-atpt 'ar-mltag-forward-atpt)
(defun ar-mltag-forward-atpt (&optional arg)
  "Moves forward over MLTAG at point if any, does nothing otherwise.
Returns end position of MLTAG "
  (interactive "p")
  (ar-th-forward 'mltag arg (called-interactively-p 'any)))

(defalias 'ar-backward-mltag-atpt 'ar-mltag-backward-atpt)
(defun ar-mltag-backward-atpt (&optional arg)
  "Moves backward over MLTAG before point if any, does nothing otherwise.
Returns beginning position of MLTAG "
  (interactive "p")
  (ar-th-backward 'mltag arg (called-interactively-p 'any)))

(defalias 'ar-transpose-mltag-atpt 'ar-mltag-transpose-atpt)
(defun ar-mltag-transpose-atpt (&optional arg)
  "Transposes MLTAG with MLTAG before point if any. "
  (interactive "*p")
  (ar-th-transpose 'mltag arg (called-interactively-p 'any)))

(defalias 'ar-sort-mltag-atpt 'ar-mltag-sort-atpt)
(defun ar-mltag-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts mltags in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'mltag reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-mltag-atpt 'ar-mltag-check-atpt)
(defun ar-mltag-check-atpt ()
  "Return t if a MLTAG at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-mltag-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-mltag-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-slashedparen-atpt (&optional arg no-delimiters)
  "Returns slashedparen at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'slashedparen arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-slashedparen-atpt 'ar-slashedparen-bounds-atpt)
(defun ar-slashedparen-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of slashedparen if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'slashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-slashedparen-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position SLASHEDPAREN at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'slashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-slashedparen-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of SLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-slashedparen-atpt 'ar-slashedparen-beginning-atpt)
(defun ar-slashedparen-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class SLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-slashedparen-atpt 'ar-slashedparen-end-atpt)
(defun ar-slashedparen-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class SLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-slashedparen-p-atpt 'ar-slashedparen-in-p-atpt)
(defun ar-slashedparen-in-p-atpt (&optional no-delimiters)
  "Returns bounds of SLASHEDPAREN at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-slashedparen-atpt 'ar-slashedparen-length-atpt)
(defun ar-slashedparen-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class SLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-slashedparen-atpt 'ar-slashedparen-copy-atpt)
(defun ar-slashedparen-copy-atpt (&optional no-delimiters)
  "Returns a copy of SLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-slashedparen-atpt 'ar-slashedparen-delete-atpt)
(defun ar-slashedparen-delete-atpt (&optional arg)
  "Deletes SLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-delete 'slashedparen arg (called-interactively-p 'any)))

(defalias 'ar-delete-slashedparen-in-region 'ar-slashedparen-delete-in-region)
(defun ar-slashedparen-delete-in-region (beg end)
  "Deletes SLASHEDPAREN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'slashedparen beg end (called-interactively-p 'any)))

(defun ar-blok-slashedparen-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around slashedparen.
  Returns blok or nil if no SLASHEDPAREN at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-slashedparen-atpt 'ar-slashedparen-backslashparen-atpt)
(defun ar-backslashparen-slashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around slashedparen at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-slashedparen-atpt 'ar-slashedparen-doublebackslash-atpt)
(defun ar-slashedparen-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around SLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-slashedparen-atpt 'ar-slashedparen-doubleslash-atpt)
(defun ar-slashedparen-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around SLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-slashedparen-atpt 'ar-slashedparen-doublebackslashparen-atpt)
(defun ar-slashedparen-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around SLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-slashedparen-atpt 'ar-slashedparen-slashparen-atpt)
(defun ar-slashedparen-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around SLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'slashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-comment-slashedparen-atpt (&optional no-delimiters)
  "Comments SLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-comment 'slashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-slashedparen-atpt (&optional no-delimiters)
  "Put a comma after SLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-commatize 'slashedparen no-delimiters (called-interactively-p 'any)))

(defun ar-quote-slashedparen-atpt (&optional no-delimiters)
  "Put a singlequote before SLASHEDPAREN at point if any. "
  (interactive "*p")
  (ar-th-quote 'slashedparen no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-slashedparen-atpt 'ar-slashedparen-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-slashedparen-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around SLASHEDPAREN at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-slashedparen-atpt 'ar-slashedparen-mark-atpt)
(defun ar-slashedparen-mark-atpt ()
  "Marks SLASHEDPAREN at point if any. "
  (interactive)
  (ar-th-mark 'slashedparen))

(defalias 'ar-hide-slashedparen-atpt 'ar-slashedparen-hide-atpt)
(defun ar-slashedparen-hide-atpt ()
  "Hides SLASHEDPAREN at point. "
  (interactive)
  (ar-th-hide 'slashedparen))

(defalias 'ar-show-slashedparen-atpt 'ar-slashedparen-show-atpt)
(defun ar-slashedparen-show-atpt ()
  "Shows hidden SLASHEDPAREN at point. "
  (interactive)
  (ar-th-show 'slashedparen))

(defalias 'ar-hide-show-slashedparen-atpt 'ar-slashedparen-hide-show-atpt)
(defun ar-slashedparen-hide-show-atpt ()
  "Alternatively hides or shows SLASHEDPAREN at point. "
  (interactive)
  (ar-th-hide-show 'slashedparen))

(defalias 'ar-highlight-slashedparen-atpt-mode 'ar-slashedparen-highlight-atpt-mode)

(defun ar-slashedparen-highlight-atpt-mode (&optional no-delimiters)
  "Toggles slashedparen-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-slashedparen-atpt 'ar-slashedparen-kill-atpt)
(defun ar-slashedparen-kill-atpt (&optional no-delimiters)
  "Kills SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-kill 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-slashedparen-atpt 'ar-slashedparen-kill-backward-atpt)
(defun ar-slashedparen-kill-backward-atpt (&optional no-delimiters)
  "Kills SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-slashedparen-atpt 'ar-slashedparen-leftrightsinglequote-atpt)
(defun ar-slashedparen-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'slashedparen no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-slashedparen-atpt 'ar-slashedparen-parentize-atpt)
;; ;;;###autoload
;; (defun ar-slashedparen-parentize-atpt (&optional no-delimiters)
;;   "Parentizes SLASHEDPAREN at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-slashedparen-atpt 'ar-slashedparen-separate-atpt)
(defun ar-slashedparen-separate-atpt (&optional no-delimiters)
  "Separates SLASHEDPAREN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'slashedparen no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-slashedparen-atpt 'ar-slashedparen-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-slashedparen-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes SLASHEDPAREN at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-slashedparen-atpt 'ar-slashedparen-triplequotedq-atpt)
(defun ar-slashedparen-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around slashedparen. "
  (interactive "*p")
  (ar-th-triplequotedq 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-slashedparen-atpt 'ar-slashedparen-triplequotesq-atpt)
(defun ar-slashedparen-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around slashedparen. "
  (interactive "*p")
  (ar-th-triplequotesq 'slashedparen no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashedparen-trim-atpt 'ar-trim-slashedparen-atpt)
(defun ar-trim-slashedparen-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'slashedparen t t))

(defalias 'ar-trim-left-slashedparen-atpt 'ar-trim-slashedparen-left-atpt)
(defun ar-trim-slashedparen-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'slashedparen t nil))

(defalias 'ar-trim-right-slashedparen-atpt 'ar-trim-slashedparen-right-atpt)
(defun ar-trim-slashedparen-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'slashedparen nil t))

(defun ar-underscore-slashedparen-atpt (&optional no-delimiters)
  "Put underscore char around SLASHEDPAREN. "
  (interactive "*p")
  (ar-th-underscore 'slashedparen no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-slashedparen-whitespace-atpt 'ar-whitespace-slashedparen-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-slashedparen-atpt (&optional no-delimiters)
;;   "Put whitespace char around SLASHEDPAREN. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'slashedparen nil t))

(defalias 'ar-forward-slashedparen-atpt 'ar-slashedparen-forward-atpt)
(defun ar-slashedparen-forward-atpt (&optional arg)
  "Moves forward over SLASHEDPAREN at point if any, does nothing otherwise.
Returns end position of SLASHEDPAREN "
  (interactive "p")
  (ar-th-forward 'slashedparen arg (called-interactively-p 'any)))

(defalias 'ar-backward-slashedparen-atpt 'ar-slashedparen-backward-atpt)
(defun ar-slashedparen-backward-atpt (&optional arg)
  "Moves backward over SLASHEDPAREN before point if any, does nothing otherwise.
Returns beginning position of SLASHEDPAREN "
  (interactive "p")
  (ar-th-backward 'slashedparen arg (called-interactively-p 'any)))

(defalias 'ar-transpose-slashedparen-atpt 'ar-slashedparen-transpose-atpt)
(defun ar-slashedparen-transpose-atpt (&optional arg)
  "Transposes SLASHEDPAREN with SLASHEDPAREN before point if any. "
  (interactive "*p")
  (ar-th-transpose 'slashedparen arg (called-interactively-p 'any)))

(defalias 'ar-sort-slashedparen-atpt 'ar-slashedparen-sort-atpt)
(defun ar-slashedparen-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts slashedparens in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'slashedparen reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-slashedparen-atpt 'ar-slashedparen-check-atpt)
(defun ar-slashedparen-check-atpt ()
  "Return t if a SLASHEDPAREN at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-slashedparen-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-slashedparen-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-tabledata-atpt (&optional arg no-delimiters)
  "Returns tabledata at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'tabledata arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-tabledata-atpt 'ar-tabledata-bounds-atpt)
(defun ar-tabledata-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of tabledata if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'tabledata no-delimiters (called-interactively-p 'any)))

(defun ar-tabledata-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position TABLEDATA at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'tabledata no-delimiters (called-interactively-p 'any)))

(defun ar-tabledata-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of TABLEDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-tabledata-atpt 'ar-tabledata-beginning-atpt)
(defun ar-tabledata-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class TABLEDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-tabledata-atpt 'ar-tabledata-end-atpt)
(defun ar-tabledata-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class TABLEDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-tabledata-p-atpt 'ar-tabledata-in-p-atpt)
(defun ar-tabledata-in-p-atpt (&optional no-delimiters)
  "Returns bounds of TABLEDATA at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-tabledata-atpt 'ar-tabledata-length-atpt)
(defun ar-tabledata-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class TABLEDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-tabledata-atpt 'ar-tabledata-copy-atpt)
(defun ar-tabledata-copy-atpt (&optional no-delimiters)
  "Returns a copy of TABLEDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-tabledata-atpt 'ar-tabledata-delete-atpt)
(defun ar-tabledata-delete-atpt (&optional arg)
  "Deletes TABLEDATA at point if any. "
  (interactive "*p")
  (ar-th-delete 'tabledata arg (called-interactively-p 'any)))

(defalias 'ar-delete-tabledata-in-region 'ar-tabledata-delete-in-region)
(defun ar-tabledata-delete-in-region (beg end)
  "Deletes TABLEDATA at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'tabledata beg end (called-interactively-p 'any)))

(defun ar-blok-tabledata-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around tabledata.
  Returns blok or nil if no TABLEDATA at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-tabledata-atpt 'ar-tabledata-backslashparen-atpt)
(defun ar-backslashparen-tabledata-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around tabledata at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-tabledata-atpt 'ar-tabledata-doublebackslash-atpt)
(defun ar-tabledata-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around TABLEDATA at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-tabledata-atpt 'ar-tabledata-doubleslash-atpt)
(defun ar-tabledata-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around TABLEDATA at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-tabledata-atpt 'ar-tabledata-doublebackslashparen-atpt)
(defun ar-tabledata-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around TABLEDATA at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-tabledata-atpt 'ar-tabledata-slashparen-atpt)
(defun ar-tabledata-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around TABLEDATA at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'tabledata no-delimiters (called-interactively-p 'any)))

(defun ar-comment-tabledata-atpt (&optional no-delimiters)
  "Comments TABLEDATA at point if any. "
  (interactive "*p")
  (ar-th-comment 'tabledata no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-tabledata-atpt (&optional no-delimiters)
  "Put a comma after TABLEDATA at point if any. "
  (interactive "*p")
  (ar-th-commatize 'tabledata no-delimiters (called-interactively-p 'any)))

(defun ar-quote-tabledata-atpt (&optional no-delimiters)
  "Put a singlequote before TABLEDATA at point if any. "
  (interactive "*p")
  (ar-th-quote 'tabledata no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-tabledata-atpt 'ar-tabledata-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-tabledata-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around TABLEDATA at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-tabledata-atpt 'ar-tabledata-mark-atpt)
(defun ar-tabledata-mark-atpt ()
  "Marks TABLEDATA at point if any. "
  (interactive)
  (ar-th-mark 'tabledata))

(defalias 'ar-hide-tabledata-atpt 'ar-tabledata-hide-atpt)
(defun ar-tabledata-hide-atpt ()
  "Hides TABLEDATA at point. "
  (interactive)
  (ar-th-hide 'tabledata))

(defalias 'ar-show-tabledata-atpt 'ar-tabledata-show-atpt)
(defun ar-tabledata-show-atpt ()
  "Shows hidden TABLEDATA at point. "
  (interactive)
  (ar-th-show 'tabledata))

(defalias 'ar-hide-show-tabledata-atpt 'ar-tabledata-hide-show-atpt)
(defun ar-tabledata-hide-show-atpt ()
  "Alternatively hides or shows TABLEDATA at point. "
  (interactive)
  (ar-th-hide-show 'tabledata))

(defalias 'ar-highlight-tabledata-atpt-mode 'ar-tabledata-highlight-atpt-mode)

(defun ar-tabledata-highlight-atpt-mode (&optional no-delimiters)
  "Toggles tabledata-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-tabledata-atpt 'ar-tabledata-kill-atpt)
(defun ar-tabledata-kill-atpt (&optional no-delimiters)
  "Kills TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-kill 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-tabledata-atpt 'ar-tabledata-kill-backward-atpt)
(defun ar-tabledata-kill-backward-atpt (&optional no-delimiters)
  "Kills TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-tabledata-atpt 'ar-tabledata-leftrightsinglequote-atpt)
(defun ar-tabledata-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'tabledata no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-tabledata-atpt 'ar-tabledata-parentize-atpt)
;; ;;;###autoload
;; (defun ar-tabledata-parentize-atpt (&optional no-delimiters)
;;   "Parentizes TABLEDATA at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-tabledata-atpt 'ar-tabledata-separate-atpt)
(defun ar-tabledata-separate-atpt (&optional no-delimiters)
  "Separates TABLEDATA at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'tabledata no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-tabledata-atpt 'ar-tabledata-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-tabledata-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes TABLEDATA at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-tabledata-atpt 'ar-tabledata-triplequotedq-atpt)
(defun ar-tabledata-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around tabledata. "
  (interactive "*p")
  (ar-th-triplequotedq 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-tabledata-atpt 'ar-tabledata-triplequotesq-atpt)
(defun ar-tabledata-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around tabledata. "
  (interactive "*p")
  (ar-th-triplequotesq 'tabledata no-delimiters (called-interactively-p 'any)))

(defalias 'ar-tabledata-trim-atpt 'ar-trim-tabledata-atpt)
(defun ar-trim-tabledata-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'tabledata t t))

(defalias 'ar-trim-left-tabledata-atpt 'ar-trim-tabledata-left-atpt)
(defun ar-trim-tabledata-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'tabledata t nil))

(defalias 'ar-trim-right-tabledata-atpt 'ar-trim-tabledata-right-atpt)
(defun ar-trim-tabledata-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'tabledata nil t))

(defun ar-underscore-tabledata-atpt (&optional no-delimiters)
  "Put underscore char around TABLEDATA. "
  (interactive "*p")
  (ar-th-underscore 'tabledata no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-tabledata-whitespace-atpt 'ar-whitespace-tabledata-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-tabledata-atpt (&optional no-delimiters)
;;   "Put whitespace char around TABLEDATA. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'tabledata nil t))

(defalias 'ar-forward-tabledata-atpt 'ar-tabledata-forward-atpt)
(defun ar-tabledata-forward-atpt (&optional arg)
  "Moves forward over TABLEDATA at point if any, does nothing otherwise.
Returns end position of TABLEDATA "
  (interactive "p")
  (ar-th-forward 'tabledata arg (called-interactively-p 'any)))

(defalias 'ar-backward-tabledata-atpt 'ar-tabledata-backward-atpt)
(defun ar-tabledata-backward-atpt (&optional arg)
  "Moves backward over TABLEDATA before point if any, does nothing otherwise.
Returns beginning position of TABLEDATA "
  (interactive "p")
  (ar-th-backward 'tabledata arg (called-interactively-p 'any)))

(defalias 'ar-transpose-tabledata-atpt 'ar-tabledata-transpose-atpt)
(defun ar-tabledata-transpose-atpt (&optional arg)
  "Transposes TABLEDATA with TABLEDATA before point if any. "
  (interactive "*p")
  (ar-th-transpose 'tabledata arg (called-interactively-p 'any)))

(defalias 'ar-sort-tabledata-atpt 'ar-tabledata-sort-atpt)
(defun ar-tabledata-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts tabledatas in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'tabledata reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-tabledata-atpt 'ar-tabledata-check-atpt)
(defun ar-tabledata-check-atpt ()
  "Return t if a TABLEDATA at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-tabledata-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-tabledata-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-xslstylesheet-atpt (&optional arg no-delimiters)
  "Returns xslstylesheet at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'xslstylesheet arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-xslstylesheet-atpt 'ar-xslstylesheet-bounds-atpt)
(defun ar-xslstylesheet-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of xslstylesheet if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defun ar-xslstylesheet-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position XSLSTYLESHEET at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defun ar-xslstylesheet-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of XSLSTYLESHEET at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-xslstylesheet-atpt 'ar-xslstylesheet-beginning-atpt)
(defun ar-xslstylesheet-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class XSLSTYLESHEET at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-xslstylesheet-atpt 'ar-xslstylesheet-end-atpt)
(defun ar-xslstylesheet-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class XSLSTYLESHEET at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-xslstylesheet-p-atpt 'ar-xslstylesheet-in-p-atpt)
(defun ar-xslstylesheet-in-p-atpt (&optional no-delimiters)
  "Returns bounds of XSLSTYLESHEET at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-xslstylesheet-atpt 'ar-xslstylesheet-length-atpt)
(defun ar-xslstylesheet-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class XSLSTYLESHEET at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-xslstylesheet-atpt 'ar-xslstylesheet-copy-atpt)
(defun ar-xslstylesheet-copy-atpt (&optional no-delimiters)
  "Returns a copy of XSLSTYLESHEET at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-xslstylesheet-atpt 'ar-xslstylesheet-delete-atpt)
(defun ar-xslstylesheet-delete-atpt (&optional arg)
  "Deletes XSLSTYLESHEET at point if any. "
  (interactive "*p")
  (ar-th-delete 'xslstylesheet arg (called-interactively-p 'any)))

(defalias 'ar-delete-xslstylesheet-in-region 'ar-xslstylesheet-delete-in-region)
(defun ar-xslstylesheet-delete-in-region (beg end)
  "Deletes XSLSTYLESHEET at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'xslstylesheet beg end (called-interactively-p 'any)))

(defun ar-blok-xslstylesheet-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around xslstylesheet.
  Returns blok or nil if no XSLSTYLESHEET at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-xslstylesheet-atpt 'ar-xslstylesheet-backslashparen-atpt)
(defun ar-backslashparen-xslstylesheet-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around xslstylesheet at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-xslstylesheet-atpt 'ar-xslstylesheet-doublebackslash-atpt)
(defun ar-xslstylesheet-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around XSLSTYLESHEET at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-xslstylesheet-atpt 'ar-xslstylesheet-doubleslash-atpt)
(defun ar-xslstylesheet-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around XSLSTYLESHEET at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-xslstylesheet-atpt 'ar-xslstylesheet-doublebackslashparen-atpt)
(defun ar-xslstylesheet-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around XSLSTYLESHEET at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-xslstylesheet-atpt 'ar-xslstylesheet-slashparen-atpt)
(defun ar-xslstylesheet-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around XSLSTYLESHEET at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defun ar-comment-xslstylesheet-atpt (&optional no-delimiters)
  "Comments XSLSTYLESHEET at point if any. "
  (interactive "*p")
  (ar-th-comment 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-xslstylesheet-atpt (&optional no-delimiters)
  "Put a comma after XSLSTYLESHEET at point if any. "
  (interactive "*p")
  (ar-th-commatize 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defun ar-quote-xslstylesheet-atpt (&optional no-delimiters)
  "Put a singlequote before XSLSTYLESHEET at point if any. "
  (interactive "*p")
  (ar-th-quote 'xslstylesheet no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-xslstylesheet-atpt 'ar-xslstylesheet-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-xslstylesheet-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around XSLSTYLESHEET at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-xslstylesheet-atpt 'ar-xslstylesheet-mark-atpt)
(defun ar-xslstylesheet-mark-atpt ()
  "Marks XSLSTYLESHEET at point if any. "
  (interactive)
  (ar-th-mark 'xslstylesheet))

(defalias 'ar-hide-xslstylesheet-atpt 'ar-xslstylesheet-hide-atpt)
(defun ar-xslstylesheet-hide-atpt ()
  "Hides XSLSTYLESHEET at point. "
  (interactive)
  (ar-th-hide 'xslstylesheet))

(defalias 'ar-show-xslstylesheet-atpt 'ar-xslstylesheet-show-atpt)
(defun ar-xslstylesheet-show-atpt ()
  "Shows hidden XSLSTYLESHEET at point. "
  (interactive)
  (ar-th-show 'xslstylesheet))

(defalias 'ar-hide-show-xslstylesheet-atpt 'ar-xslstylesheet-hide-show-atpt)
(defun ar-xslstylesheet-hide-show-atpt ()
  "Alternatively hides or shows XSLSTYLESHEET at point. "
  (interactive)
  (ar-th-hide-show 'xslstylesheet))

(defalias 'ar-highlight-xslstylesheet-atpt-mode 'ar-xslstylesheet-highlight-atpt-mode)

(defun ar-xslstylesheet-highlight-atpt-mode (&optional no-delimiters)
  "Toggles xslstylesheet-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-xslstylesheet-atpt 'ar-xslstylesheet-kill-atpt)
(defun ar-xslstylesheet-kill-atpt (&optional no-delimiters)
  "Kills XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-kill 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-xslstylesheet-atpt 'ar-xslstylesheet-kill-backward-atpt)
(defun ar-xslstylesheet-kill-backward-atpt (&optional no-delimiters)
  "Kills XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-xslstylesheet-atpt 'ar-xslstylesheet-leftrightsinglequote-atpt)
(defun ar-xslstylesheet-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'xslstylesheet no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-xslstylesheet-atpt 'ar-xslstylesheet-parentize-atpt)
;; ;;;###autoload
;; (defun ar-xslstylesheet-parentize-atpt (&optional no-delimiters)
;;   "Parentizes XSLSTYLESHEET at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-xslstylesheet-atpt 'ar-xslstylesheet-separate-atpt)
(defun ar-xslstylesheet-separate-atpt (&optional no-delimiters)
  "Separates XSLSTYLESHEET at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'xslstylesheet no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-xslstylesheet-atpt 'ar-xslstylesheet-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-xslstylesheet-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes XSLSTYLESHEET at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-xslstylesheet-atpt 'ar-xslstylesheet-triplequotedq-atpt)
(defun ar-xslstylesheet-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around xslstylesheet. "
  (interactive "*p")
  (ar-th-triplequotedq 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-xslstylesheet-atpt 'ar-xslstylesheet-triplequotesq-atpt)
(defun ar-xslstylesheet-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around xslstylesheet. "
  (interactive "*p")
  (ar-th-triplequotesq 'xslstylesheet no-delimiters (called-interactively-p 'any)))

(defalias 'ar-xslstylesheet-trim-atpt 'ar-trim-xslstylesheet-atpt)
(defun ar-trim-xslstylesheet-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'xslstylesheet t t))

(defalias 'ar-trim-left-xslstylesheet-atpt 'ar-trim-xslstylesheet-left-atpt)
(defun ar-trim-xslstylesheet-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'xslstylesheet t nil))

(defalias 'ar-trim-right-xslstylesheet-atpt 'ar-trim-xslstylesheet-right-atpt)
(defun ar-trim-xslstylesheet-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'xslstylesheet nil t))

(defun ar-underscore-xslstylesheet-atpt (&optional no-delimiters)
  "Put underscore char around XSLSTYLESHEET. "
  (interactive "*p")
  (ar-th-underscore 'xslstylesheet no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-xslstylesheet-whitespace-atpt 'ar-whitespace-xslstylesheet-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-xslstylesheet-atpt (&optional no-delimiters)
;;   "Put whitespace char around XSLSTYLESHEET. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'xslstylesheet nil t))

(defalias 'ar-forward-xslstylesheet-atpt 'ar-xslstylesheet-forward-atpt)
(defun ar-xslstylesheet-forward-atpt (&optional arg)
  "Moves forward over XSLSTYLESHEET at point if any, does nothing otherwise.
Returns end position of XSLSTYLESHEET "
  (interactive "p")
  (ar-th-forward 'xslstylesheet arg (called-interactively-p 'any)))

(defalias 'ar-backward-xslstylesheet-atpt 'ar-xslstylesheet-backward-atpt)
(defun ar-xslstylesheet-backward-atpt (&optional arg)
  "Moves backward over XSLSTYLESHEET before point if any, does nothing otherwise.
Returns beginning position of XSLSTYLESHEET "
  (interactive "p")
  (ar-th-backward 'xslstylesheet arg (called-interactively-p 'any)))

(defalias 'ar-transpose-xslstylesheet-atpt 'ar-xslstylesheet-transpose-atpt)
(defun ar-xslstylesheet-transpose-atpt (&optional arg)
  "Transposes XSLSTYLESHEET with XSLSTYLESHEET before point if any. "
  (interactive "*p")
  (ar-th-transpose 'xslstylesheet arg (called-interactively-p 'any)))

(defalias 'ar-sort-xslstylesheet-atpt 'ar-xslstylesheet-sort-atpt)
(defun ar-xslstylesheet-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts xslstylesheets in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'xslstylesheet reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-xslstylesheet-atpt 'ar-xslstylesheet-check-atpt)
(defun ar-xslstylesheet-check-atpt ()
  "Return t if a XSLSTYLESHEET at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-xslstylesheet-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-xslstylesheet-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

(defun ar-xsltemplate-atpt (&optional arg no-delimiters)
  "Returns xsltemplate at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'xsltemplate arg no-delimiters (called-interactively-p 'any)))

(defalias 'ar-bounds-of-xsltemplate-atpt 'ar-xsltemplate-bounds-atpt)
(defun ar-xsltemplate-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of xsltemplate if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defun ar-xsltemplate-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position XSLTEMPLATE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defun ar-xsltemplate-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of XSLTEMPLATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-beginning-of-xsltemplate-atpt 'ar-xsltemplate-beginning-atpt)
(defun ar-xsltemplate-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class XSLTEMPLATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-end-of-xsltemplate-atpt 'ar-xsltemplate-end-atpt)
(defun ar-xsltemplate-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class XSLTEMPLATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-in-xsltemplate-p-atpt 'ar-xsltemplate-in-p-atpt)
(defun ar-xsltemplate-in-p-atpt (&optional no-delimiters)
  "Returns bounds of XSLTEMPLATE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-length-of-xsltemplate-atpt 'ar-xsltemplate-length-atpt)
(defun ar-xsltemplate-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class XSLTEMPLATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-copy-xsltemplate-atpt 'ar-xsltemplate-copy-atpt)
(defun ar-xsltemplate-copy-atpt (&optional no-delimiters)
  "Returns a copy of XSLTEMPLATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-delete-xsltemplate-atpt 'ar-xsltemplate-delete-atpt)
(defun ar-xsltemplate-delete-atpt (&optional arg)
  "Deletes XSLTEMPLATE at point if any. "
  (interactive "*p")
  (ar-th-delete 'xsltemplate arg (called-interactively-p 'any)))

(defalias 'ar-delete-xsltemplate-in-region 'ar-xsltemplate-delete-in-region)
(defun ar-xsltemplate-delete-in-region (beg end)
  "Deletes XSLTEMPLATE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'xsltemplate beg end (called-interactively-p 'any)))

(defun ar-blok-xsltemplate-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around xsltemplate.
  Returns blok or nil if no XSLTEMPLATE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-backslashparen-xsltemplate-atpt 'ar-xsltemplate-backslashparen-atpt)
(defun ar-backslashparen-xsltemplate-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around xsltemplate at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-backslashparen 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslash-xsltemplate-atpt 'ar-xsltemplate-doublebackslash-atpt)
(defun ar-xsltemplate-doublebackslash-atpt (&optional no-delimiters)
  "Puts doubled backslashes around XSLTEMPLATE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doubleslash-xsltemplate-atpt 'ar-xsltemplate-doubleslash-atpt)
(defun ar-xsltemplate-doubleslash-atpt (&optional no-delimiters)
  "Puts doubled slashes around XSLTEMPLATE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-doublebackslashparen-xsltemplate-atpt 'ar-xsltemplate-doublebackslashparen-atpt)
(defun ar-xsltemplate-doublebackslashparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around XSLTEMPLATE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-slashparen-xsltemplate-atpt 'ar-xsltemplate-slashparen-atpt)
(defun ar-xsltemplate-slashparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around XSLTEMPLATE at point if any. "
  (interactive "*p")
  (ar-th-slashparen 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defun ar-comment-xsltemplate-atpt (&optional no-delimiters)
  "Comments XSLTEMPLATE at point if any. "
  (interactive "*p")
  (ar-th-comment 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defun ar-commatize-xsltemplate-atpt (&optional no-delimiters)
  "Put a comma after XSLTEMPLATE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defun ar-quote-xsltemplate-atpt (&optional no-delimiters)
  "Put a singlequote before XSLTEMPLATE at point if any. "
  (interactive "*p")
  (ar-th-quote 'xsltemplate no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-hyphen-xsltemplate-atpt 'ar-xsltemplate-hyphen-atpt)
;; ;;;###autoload
;; (defun ar-xsltemplate-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around XSLTEMPLATE at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-mark-xsltemplate-atpt 'ar-xsltemplate-mark-atpt)
(defun ar-xsltemplate-mark-atpt ()
  "Marks XSLTEMPLATE at point if any. "
  (interactive)
  (ar-th-mark 'xsltemplate))

(defalias 'ar-hide-xsltemplate-atpt 'ar-xsltemplate-hide-atpt)
(defun ar-xsltemplate-hide-atpt ()
  "Hides XSLTEMPLATE at point. "
  (interactive)
  (ar-th-hide 'xsltemplate))

(defalias 'ar-show-xsltemplate-atpt 'ar-xsltemplate-show-atpt)
(defun ar-xsltemplate-show-atpt ()
  "Shows hidden XSLTEMPLATE at point. "
  (interactive)
  (ar-th-show 'xsltemplate))

(defalias 'ar-hide-show-xsltemplate-atpt 'ar-xsltemplate-hide-show-atpt)
(defun ar-xsltemplate-hide-show-atpt ()
  "Alternatively hides or shows XSLTEMPLATE at point. "
  (interactive)
  (ar-th-hide-show 'xsltemplate))

(defalias 'ar-highlight-xsltemplate-atpt-mode 'ar-xsltemplate-highlight-atpt-mode)

(defun ar-xsltemplate-highlight-atpt-mode (&optional no-delimiters)
  "Toggles xsltemplate-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-xsltemplate-atpt 'ar-xsltemplate-kill-atpt)
(defun ar-xsltemplate-kill-atpt (&optional no-delimiters)
  "Kills XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-kill 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-kill-backward-xsltemplate-atpt 'ar-xsltemplate-kill-backward-atpt)
(defun ar-xsltemplate-kill-backward-atpt (&optional no-delimiters)
  "Kills XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-leftrightsinglequote-xsltemplate-atpt 'ar-xsltemplate-leftrightsinglequote-atpt)
(defun ar-xsltemplate-leftrightsinglequote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*p")
  (ar-th-leftrightsinglequote 'xsltemplate no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-parentize-xsltemplate-atpt 'ar-xsltemplate-parentize-atpt)
;; ;;;###autoload
;; (defun ar-xsltemplate-parentize-atpt (&optional no-delimiters)
;;   "Parentizes XSLTEMPLATE at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-separate-xsltemplate-atpt 'ar-xsltemplate-separate-atpt)
(defun ar-xsltemplate-separate-atpt (&optional no-delimiters)
  "Separates XSLTEMPLATE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'xsltemplate no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-singlequote-xsltemplate-atpt 'ar-xsltemplate-singlequote-atpt)
;; ;;;###autoload
;; (defun ar-xsltemplate-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes XSLTEMPLATE at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotedq-xsltemplate-atpt 'ar-xsltemplate-triplequotedq-atpt)
(defun ar-xsltemplate-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around xsltemplate. "
  (interactive "*p")
  (ar-th-triplequotedq 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-triplequotesq-xsltemplate-atpt 'ar-xsltemplate-triplequotesq-atpt)
(defun ar-xsltemplate-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around xsltemplate. "
  (interactive "*p")
  (ar-th-triplequotesq 'xsltemplate no-delimiters (called-interactively-p 'any)))

(defalias 'ar-xsltemplate-trim-atpt 'ar-trim-xsltemplate-atpt)
(defun ar-trim-xsltemplate-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'xsltemplate t t))

(defalias 'ar-trim-left-xsltemplate-atpt 'ar-trim-xsltemplate-left-atpt)
(defun ar-trim-xsltemplate-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'xsltemplate t nil))

(defalias 'ar-trim-right-xsltemplate-atpt 'ar-trim-xsltemplate-right-atpt)
(defun ar-trim-xsltemplate-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'xsltemplate nil t))

(defun ar-underscore-xsltemplate-atpt (&optional no-delimiters)
  "Put underscore char around XSLTEMPLATE. "
  (interactive "*p")
  (ar-th-underscore 'xsltemplate no-delimiters (called-interactively-p 'any)))

;; (defalias 'ar-xsltemplate-whitespace-atpt 'ar-whitespace-xsltemplate-atpt)
;; ;;;###autoload
;; (defun ar-whitespace-xsltemplate-atpt (&optional no-delimiters)
;;   "Put whitespace char around XSLTEMPLATE. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'xsltemplate nil t))

(defalias 'ar-forward-xsltemplate-atpt 'ar-xsltemplate-forward-atpt)
(defun ar-xsltemplate-forward-atpt (&optional arg)
  "Moves forward over XSLTEMPLATE at point if any, does nothing otherwise.
Returns end position of XSLTEMPLATE "
  (interactive "p")
  (ar-th-forward 'xsltemplate arg (called-interactively-p 'any)))

(defalias 'ar-backward-xsltemplate-atpt 'ar-xsltemplate-backward-atpt)
(defun ar-xsltemplate-backward-atpt (&optional arg)
  "Moves backward over XSLTEMPLATE before point if any, does nothing otherwise.
Returns beginning position of XSLTEMPLATE "
  (interactive "p")
  (ar-th-backward 'xsltemplate arg (called-interactively-p 'any)))

(defalias 'ar-transpose-xsltemplate-atpt 'ar-xsltemplate-transpose-atpt)
(defun ar-xsltemplate-transpose-atpt (&optional arg)
  "Transposes XSLTEMPLATE with XSLTEMPLATE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'xsltemplate arg (called-interactively-p 'any)))

(defalias 'ar-sort-xsltemplate-atpt 'ar-xsltemplate-sort-atpt)
(defun ar-xsltemplate-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts xsltemplates in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'xsltemplate reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-xsltemplate-atpt 'ar-xsltemplate-check-atpt)
(defun ar-xsltemplate-check-atpt ()
  "Return t if a XSLTEMPLATE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-xsltemplate-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-xsltemplate-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (called-interactively-p 'any) (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-markup-list end



(put 'mlattribut 'beginning-op-at
     (lambda ()
       (let* ((orig (point))
              (erg (parse-partial-sexp (point-min) (point)))
              (beg (nth 8 erg)))
         (cond (beg
                (goto-char beg)
                (when (search-backward "=" nil t 1)
                  (forward-char -1))
                (skip-chars-backward " \t\r\n\f")
                (ar-th-gotobeg 'symbol))
               ((looking-at "[\" \t\r\n\f=]+")
                (ar-go-in-statement-backward-to-beg-of-symbol))
               (t (ar-th-gotoend 'symbol)
                  (if (looking-at "[ \t\r\n\f]*=")
                      (ar-th-gotobeg 'symbol)
                    (goto-char orig)))))))

(put 'mlattribut 'end-op-at
     (lambda ()
       (let ((orig (point)))
         (when (eq (point) (ar-th-gotobeg 'symbol))
           (ar-end-of-symbol-atpt)
           (if (looking-at "[ \t\r\n\f]*=")
               (search-forward "\"" nil t 2)
             (goto-char orig))))))

(defun ar-go-in-statement-backward ()
  (interactive)
  (skip-chars-backward "=\" \t\r\n\f")
  (forward-char -1))

(defun ar-go-in-statement-backward-to-beg-of-symbol ()
  (ar-go-in-statement-backward)
  (ar-th-gotobeg 'symbol))

(defun ar-value-of-mlattribut-atpt (&optional arg)
  (interactive "P")
  "Return the value of attribute under cursor if any.
With \\[universal-argument] its returned without doublequotes"
  (ar-end-of-mlattribut-atpt)
  (forward-char -1)
  (let ((value (ar-string-atpt arg)))
    (when (interactive-p) (message "%s" value))
    value))

(defun ar-name-of-mlattribut-atpt ()
  (interactive)
  (ar-beginning-of-mlattribut-atpt)
  (let ((name (ar-alnum-atpt)))
    (when (interactive-p) (message "%s" name))
    name))

(defun ar-mlattribut-or-name-atpt (&optional arg)
  "Returns mlattribut-atpt at point if any, nil otherwise.
  With \C-u ar-name-of-mlattribut-atpt is called "
  (interactive "P")
  (if (eq 4 (prefix-numeric-value arg))
    (ar-name-of-mlattribut-atpt)
  (ar-th 'mlattribut arg (interactive-p))))




(provide 'thing-at-point-markup)
;;; thing-at-point-markup.el ends here
