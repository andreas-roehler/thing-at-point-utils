;;; thing-at-point-markup.el --- th-at-point edit functions -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2024 Andreas Röhler, unless
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
(require 'ar-thingatpt-basic-definitions)
;; ar-thing-at-point-utils-markup ar-atpt-markup-list start

(defun ar-un-beginendquote-atpt (&optional arg)
  "Remove markups provided by beginendquote. "
  (interactive "P*")
  (ar-th-un-ml 'beginendquote arg arg))

(defun ar-un-blok-atpt (&optional arg)
  "Remove markups provided by blok. "
  (interactive "P*")
  (ar-th-un-ml 'blok arg arg))

(defun ar-un-doublebackslashed-atpt (&optional arg)
  "Remove markups provided by doublebackslashed. "
  (interactive "P*")
  (ar-th-un-ml 'doublebackslashed arg arg))

(defun ar-un-doublebackslashedparen-atpt (&optional arg)
  "Remove markups provided by doublebackslashedparen. "
  (interactive "P*")
  (ar-th-un-ml 'doublebackslashedparen arg arg))

(defun ar-un-doubleslashed-atpt (&optional arg)
  "Remove markups provided by doubleslashed. "
  (interactive "P*")
  (ar-th-un-ml 'doubleslashed arg arg))

(defun ar-un-tabledata-atpt (&optional arg)
  "Remove markups provided by tabledata. "
  (interactive "P*")
  (ar-th-un-ml 'tabledata arg arg))

;; ar-thing-at-point-utils-markup: ar-atpt-markup-list end


;; ar-thing-at-point-utils-delimiters-core ar-atpt-markup-list start

(defun ar-beginendquote-atpt (&optional no-delimiters)
  "Returns beginendquote at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'beginendquote no-delimiters))

(defun ar-bounds-of-beginendquote-atpt (&optional no-delimiters)
  "Returns a list, borders of beginendquote if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'beginendquote no-delimiters))

(defun ar-beginendquote-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BEGINENDQUOTE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'beginendquote no-delimiters))

(defun ar-beginendquote-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BEGINENDQUOTE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'beginendquote no-delimiters))

(defun ar-beginning-of-beginendquote-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class BEGINENDQUOTE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'beginendquote no-delimiters))

(defun ar-end-of-beginendquote-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class BEGINENDQUOTE at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'beginendquote no-delimiters))

(defun ar-in-beginendquote-p-atpt (&optional no-delimiters)
  "Returns bounds of BEGINENDQUOTE at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'beginendquote no-delimiters))

(defun ar-length-of-beginendquote-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class BEGINENDQUOTE at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'beginendquote no-delimiters))

(defun ar-copy-beginendquote-atpt (&optional no-delimiters)
  "Returns a copy of BEGINENDQUOTE at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'beginendquote no-delimiters))

(defun ar-delete-beginendquote-atpt (&optional no-delimiters)
  "Deletes BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-delete 'beginendquote no-delimiters))

(defun ar-delete-beginendquote-in-region (beg end)
  "Deletes BEGINENDQUOTE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'beginendquote beg end))

(defun ar-blok-beginendquote-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around beginendquote.
  Returns blok or nil if no BEGINENDQUOTE at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'beginendquote no-delimiters))

(defun ar-backslashparen-beginendquote-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around beginendquote at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'beginendquote no-delimiters))

(defun ar-doublebackslash-beginendquote-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'beginendquote no-delimiters))

(defun ar-doubleslash-beginendquote-atpt (&optional no-delimiters)
  "Puts doubled slashes around BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'beginendquote no-delimiters))

(defun ar-doublebackslashparen-beginendquote-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'beginendquote no-delimiters))

(defun ar-doublebacktick-beginendquote-atpt (&optional no-delimiters)
  "Provides double backticks around BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'beginendquote no-delimiters))

(defun ar-slashparen-beginendquote-atpt (&optional no-delimiters)
  "Provides slashed parentheses around BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'beginendquote no-delimiters))

(defun ar-slashparen-beginendquote-atpt (&optional no-delimiters)
  "Provides slashed parentheses around BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'beginendquote no-delimiters))

(defun ar-comment-beginendquote-atpt (&optional no-delimiters)
  "Comments BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-comment 'beginendquote no-delimiters))

(defun ar-commatize-beginendquote-atpt (&optional no-delimiters)
  "Put a comma after BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-commatize 'beginendquote no-delimiters))

(defun ar-quote-beginendquote-atpt (&optional no-delimiters)
  "Put a singlequote before BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-quote 'beginendquote no-delimiters))


(defun ar-mark-beginendquote-atpt (&optional no-delimiters)
  "Marks BEGINENDQUOTE at point if any. "
  (interactive "p")
  (ar-th-mark 'beginendquote))

(defun ar-hide-beginendquote-atpt (&optional no-delimiters)
  "Hides BEGINENDQUOTE at point. "
  (interactive "p")
  (ar-th-hide 'beginendquote))

(defun ar-show-beginendquote-atpt (&optional no-delimiters)
  "Shows hidden BEGINENDQUOTE at point. "
  (interactive "p")
  (ar-th-show 'beginendquote))

(defun ar-hide-show-beginendquote-atpt (&optional no-delimiters)
  "Alternatively hides or shows BEGINENDQUOTE at point. "
  (interactive "p")
  (ar-th-hide-show 'beginendquote))

(defun ar-highlight-beginendquote-atpt-mode (&optional no-delimiters)
  "Toggles beginendquote-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'beginendquote no-delimiters))

(defun ar-kill-beginendquote-atpt (&optional no-delimiters)
  "Kills BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-kill 'beginendquote no-delimiters))

(defun ar-curvedsinglequote-beginendquote-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'beginendquote no-delimiters))

(defun ar-separate-beginendquote-atpt (&optional no-delimiters)
  "Separates BEGINENDQUOTE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'beginendquote no-delimiters))

(defun ar-triplequotedq-beginendquote-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around beginendquote. "
  (interactive "*P")
  (ar-th-triplequotedq 'beginendquote no-delimiters))

(defun ar-triplequotesq-beginendquote-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around beginendquote. "
  (interactive "*P")
  (ar-th-triplequotesq 'beginendquote no-delimiters))

(defun ar-triplebacktick-beginendquote-atpt (&optional no-delimiters)
  "Deletes beginendquote at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'beginendquote no-delimiters))

(defun ar-trim-beginendquote-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'beginendquote no-delimiters t t))

(defun ar-left-trim-beginendquote-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'beginendquote no-delimiters t))

(defun ar-right-trim-beginendquote-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'beginendquote n no-delimiters nil t))

(defun ar-underscore-beginendquote-atpt (&optional no-delimiters)
  "Put underscore char around BEGINENDQUOTE. "
  (interactive "*P")
  (ar-th-underscore 'beginendquote no-delimiters))

(defun ar-forward-beginendquote-atpt (&optional no-delimiters)
  "Moves forward over BEGINENDQUOTE at point if any, does nothing otherwise.
Returns end position of BEGINENDQUOTE "
  (interactive "p")
  (ar-th-forward 'beginendquote no-delimiters))

(defun ar-backward-beginendquote-atpt (&optional no-delimiters)
  "Moves backward over BEGINENDQUOTE before point if any, does nothing otherwise.
Returns beginning position of BEGINENDQUOTE "
  (interactive "p")
  (ar-th-backward 'beginendquote no-delimiters))

(defun ar-transpose-beginendquote-atpt (&optional no-delimiters)
  "Transposes BEGINENDQUOTE with BEGINENDQUOTE before point if any. "
  (interactive "*P")
  (ar-th-transpose 'beginendquote no-delimiters))

(defun ar-sort-beginendquote-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts beginendquotes in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'beginendquote reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-beginendquote-atpt (&optional arg)
  "Return t if a BEGINENDQUOTE at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-beginendquote-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-beginendquote-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-blok-atpt (&optional no-delimiters)
  "Returns blok at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'blok no-delimiters))

(defun ar-bounds-of-blok-atpt (&optional no-delimiters)
  "Returns a list, borders of blok if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'blok no-delimiters))

(defun ar-blok-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BLOK at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'blok no-delimiters))

(defun ar-blok-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BLOK at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'blok no-delimiters))

(defun ar-beginning-of-blok-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class BLOK at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'blok no-delimiters))

(defun ar-end-of-blok-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class BLOK at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'blok no-delimiters))

(defun ar-in-blok-p-atpt (&optional no-delimiters)
  "Returns bounds of BLOK at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'blok no-delimiters))

(defun ar-length-of-blok-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class BLOK at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'blok no-delimiters))

(defun ar-copy-blok-atpt (&optional no-delimiters)
  "Returns a copy of BLOK at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'blok no-delimiters))

(defun ar-delete-blok-atpt (&optional no-delimiters)
  "Deletes BLOK at point if any. "
  (interactive "*P")
  (ar-th-delete 'blok no-delimiters))

(defun ar-delete-blok-in-region (beg end)
  "Deletes BLOK at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'blok beg end))

(defun ar-blok-blok-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around blok.
  Returns blok or nil if no BLOK at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'blok no-delimiters))

(defun ar-backslashparen-blok-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around blok at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'blok no-delimiters))

(defun ar-doublebackslash-blok-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BLOK at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'blok no-delimiters))

(defun ar-doubleslash-blok-atpt (&optional no-delimiters)
  "Puts doubled slashes around BLOK at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'blok no-delimiters))

(defun ar-doublebackslashparen-blok-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BLOK at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'blok no-delimiters))

(defun ar-doublebacktick-blok-atpt (&optional no-delimiters)
  "Provides double backticks around BLOK at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'blok no-delimiters))

(defun ar-slashparen-blok-atpt (&optional no-delimiters)
  "Provides slashed parentheses around BLOK at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'blok no-delimiters))

(defun ar-slashparen-blok-atpt (&optional no-delimiters)
  "Provides slashed parentheses around BLOK at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'blok no-delimiters))

(defun ar-comment-blok-atpt (&optional no-delimiters)
  "Comments BLOK at point if any. "
  (interactive "*P")
  (ar-th-comment 'blok no-delimiters))

(defun ar-commatize-blok-atpt (&optional no-delimiters)
  "Put a comma after BLOK at point if any. "
  (interactive "*P")
  (ar-th-commatize 'blok no-delimiters))

(defun ar-quote-blok-atpt (&optional no-delimiters)
  "Put a singlequote before BLOK at point if any. "
  (interactive "*P")
  (ar-th-quote 'blok no-delimiters))


(defun ar-mark-blok-atpt (&optional no-delimiters)
  "Marks BLOK at point if any. "
  (interactive "p")
  (ar-th-mark 'blok))

(defun ar-hide-blok-atpt (&optional no-delimiters)
  "Hides BLOK at point. "
  (interactive "p")
  (ar-th-hide 'blok))

(defun ar-show-blok-atpt (&optional no-delimiters)
  "Shows hidden BLOK at point. "
  (interactive "p")
  (ar-th-show 'blok))

(defun ar-hide-show-blok-atpt (&optional no-delimiters)
  "Alternatively hides or shows BLOK at point. "
  (interactive "p")
  (ar-th-hide-show 'blok))

(defun ar-highlight-blok-atpt-mode (&optional no-delimiters)
  "Toggles blok-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'blok no-delimiters))

(defun ar-kill-blok-atpt (&optional no-delimiters)
  "Kills BLOK at point if any. "
  (interactive "*P")
  (ar-th-kill 'blok no-delimiters))

(defun ar-curvedsinglequote-blok-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'blok no-delimiters))

(defun ar-separate-blok-atpt (&optional no-delimiters)
  "Separates BLOK at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'blok no-delimiters))

(defun ar-triplequotedq-blok-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around blok. "
  (interactive "*P")
  (ar-th-triplequotedq 'blok no-delimiters))

(defun ar-triplequotesq-blok-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around blok. "
  (interactive "*P")
  (ar-th-triplequotesq 'blok no-delimiters))

(defun ar-triplebacktick-blok-atpt (&optional no-delimiters)
  "Deletes blok at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'blok no-delimiters))

(defun ar-trim-blok-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'blok no-delimiters t t))

(defun ar-left-trim-blok-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'blok no-delimiters t))

(defun ar-right-trim-blok-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'blok n no-delimiters nil t))

(defun ar-underscore-blok-atpt (&optional no-delimiters)
  "Put underscore char around BLOK. "
  (interactive "*P")
  (ar-th-underscore 'blok no-delimiters))

(defun ar-forward-blok-atpt (&optional no-delimiters)
  "Moves forward over BLOK at point if any, does nothing otherwise.
Returns end position of BLOK "
  (interactive "p")
  (ar-th-forward 'blok no-delimiters))

(defun ar-backward-blok-atpt (&optional no-delimiters)
  "Moves backward over BLOK before point if any, does nothing otherwise.
Returns beginning position of BLOK "
  (interactive "p")
  (ar-th-backward 'blok no-delimiters))

(defun ar-transpose-blok-atpt (&optional no-delimiters)
  "Transposes BLOK with BLOK before point if any. "
  (interactive "*P")
  (ar-th-transpose 'blok no-delimiters))

(defun ar-sort-blok-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts bloks in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'blok reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-blok-atpt (&optional arg)
  "Return t if a BLOK at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-blok-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-blok-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-doublebackslashed-atpt (&optional no-delimiters)
  "Returns doublebackslashed at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'doublebackslashed no-delimiters))

(defun ar-bounds-of-doublebackslashed-atpt (&optional no-delimiters)
  "Returns a list, borders of doublebackslashed if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'doublebackslashed no-delimiters))

(defun ar-doublebackslashed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLEBACKSLASHED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'doublebackslashed no-delimiters))

(defun ar-doublebackslashed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLEBACKSLASHED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'doublebackslashed no-delimiters))

(defun ar-beginning-of-doublebackslashed-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DOUBLEBACKSLASHED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'doublebackslashed no-delimiters))

(defun ar-end-of-doublebackslashed-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DOUBLEBACKSLASHED at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'doublebackslashed no-delimiters))

(defun ar-in-doublebackslashed-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLEBACKSLASHED at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'doublebackslashed no-delimiters))

(defun ar-length-of-doublebackslashed-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DOUBLEBACKSLASHED at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'doublebackslashed no-delimiters))

(defun ar-copy-doublebackslashed-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLEBACKSLASHED at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'doublebackslashed no-delimiters))

(defun ar-delete-doublebackslashed-atpt (&optional no-delimiters)
  "Deletes DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-delete 'doublebackslashed no-delimiters))

(defun ar-delete-doublebackslashed-in-region (beg end)
  "Deletes DOUBLEBACKSLASHED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doublebackslashed beg end))

(defun ar-blok-doublebackslashed-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around doublebackslashed.
  Returns blok or nil if no DOUBLEBACKSLASHED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'doublebackslashed no-delimiters))

(defun ar-backslashparen-doublebackslashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doublebackslashed at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'doublebackslashed no-delimiters))

(defun ar-doublebackslash-doublebackslashed-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'doublebackslashed no-delimiters))

(defun ar-doubleslash-doublebackslashed-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'doublebackslashed no-delimiters))

(defun ar-doublebackslashparen-doublebackslashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'doublebackslashed no-delimiters))

(defun ar-doublebacktick-doublebackslashed-atpt (&optional no-delimiters)
  "Provides double backticks around DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'doublebackslashed no-delimiters))

(defun ar-slashparen-doublebackslashed-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doublebackslashed no-delimiters))

(defun ar-slashparen-doublebackslashed-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doublebackslashed no-delimiters))

(defun ar-comment-doublebackslashed-atpt (&optional no-delimiters)
  "Comments DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-comment 'doublebackslashed no-delimiters))

(defun ar-commatize-doublebackslashed-atpt (&optional no-delimiters)
  "Put a comma after DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'doublebackslashed no-delimiters))

(defun ar-quote-doublebackslashed-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-quote 'doublebackslashed no-delimiters))


(defun ar-mark-doublebackslashed-atpt (&optional no-delimiters)
  "Marks DOUBLEBACKSLASHED at point if any. "
  (interactive "p")
  (ar-th-mark 'doublebackslashed))

(defun ar-hide-doublebackslashed-atpt (&optional no-delimiters)
  "Hides DOUBLEBACKSLASHED at point. "
  (interactive "p")
  (ar-th-hide 'doublebackslashed))

(defun ar-show-doublebackslashed-atpt (&optional no-delimiters)
  "Shows hidden DOUBLEBACKSLASHED at point. "
  (interactive "p")
  (ar-th-show 'doublebackslashed))

(defun ar-hide-show-doublebackslashed-atpt (&optional no-delimiters)
  "Alternatively hides or shows DOUBLEBACKSLASHED at point. "
  (interactive "p")
  (ar-th-hide-show 'doublebackslashed))

(defun ar-highlight-doublebackslashed-atpt-mode (&optional no-delimiters)
  "Toggles doublebackslashed-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'doublebackslashed no-delimiters))

(defun ar-kill-doublebackslashed-atpt (&optional no-delimiters)
  "Kills DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-kill 'doublebackslashed no-delimiters))

(defun ar-curvedsinglequote-doublebackslashed-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'doublebackslashed no-delimiters))

(defun ar-separate-doublebackslashed-atpt (&optional no-delimiters)
  "Separates DOUBLEBACKSLASHED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'doublebackslashed no-delimiters))

(defun ar-triplequotedq-doublebackslashed-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doublebackslashed. "
  (interactive "*P")
  (ar-th-triplequotedq 'doublebackslashed no-delimiters))

(defun ar-triplequotesq-doublebackslashed-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doublebackslashed. "
  (interactive "*P")
  (ar-th-triplequotesq 'doublebackslashed no-delimiters))

(defun ar-triplebacktick-doublebackslashed-atpt (&optional no-delimiters)
  "Deletes doublebackslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'doublebackslashed no-delimiters))

(defun ar-trim-doublebackslashed-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashed no-delimiters t t))

(defun ar-left-trim-doublebackslashed-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashed no-delimiters t))

(defun ar-right-trim-doublebackslashed-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashed n no-delimiters nil t))

(defun ar-underscore-doublebackslashed-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLEBACKSLASHED. "
  (interactive "*P")
  (ar-th-underscore 'doublebackslashed no-delimiters))

(defun ar-forward-doublebackslashed-atpt (&optional no-delimiters)
  "Moves forward over DOUBLEBACKSLASHED at point if any, does nothing otherwise.
Returns end position of DOUBLEBACKSLASHED "
  (interactive "p")
  (ar-th-forward 'doublebackslashed no-delimiters))

(defun ar-backward-doublebackslashed-atpt (&optional no-delimiters)
  "Moves backward over DOUBLEBACKSLASHED before point if any, does nothing otherwise.
Returns beginning position of DOUBLEBACKSLASHED "
  (interactive "p")
  (ar-th-backward 'doublebackslashed no-delimiters))

(defun ar-transpose-doublebackslashed-atpt (&optional no-delimiters)
  "Transposes DOUBLEBACKSLASHED with DOUBLEBACKSLASHED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'doublebackslashed no-delimiters))

(defun ar-sort-doublebackslashed-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts doublebackslasheds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'doublebackslashed reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-doublebackslashed-atpt (&optional arg)
  "Return t if a DOUBLEBACKSLASHED at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-doublebackslashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doublebackslashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-doublebackticked-atpt (&optional no-delimiters)
  "Returns doublebackticked at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'doublebackticked no-delimiters))

(defun ar-bounds-of-doublebackticked-atpt (&optional no-delimiters)
  "Returns a list, borders of doublebackticked if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'doublebackticked no-delimiters))

(defun ar-doublebackticked-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLEBACKTICKED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'doublebackticked no-delimiters))

(defun ar-doublebackticked-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLEBACKTICKED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'doublebackticked no-delimiters))

(defun ar-beginning-of-doublebackticked-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DOUBLEBACKTICKED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'doublebackticked no-delimiters))

(defun ar-end-of-doublebackticked-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DOUBLEBACKTICKED at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'doublebackticked no-delimiters))

(defun ar-in-doublebackticked-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLEBACKTICKED at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'doublebackticked no-delimiters))

(defun ar-length-of-doublebackticked-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DOUBLEBACKTICKED at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'doublebackticked no-delimiters))

(defun ar-copy-doublebackticked-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLEBACKTICKED at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'doublebackticked no-delimiters))

(defun ar-delete-doublebackticked-atpt (&optional no-delimiters)
  "Deletes DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-delete 'doublebackticked no-delimiters))

(defun ar-delete-doublebackticked-in-region (beg end)
  "Deletes DOUBLEBACKTICKED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doublebackticked beg end))

(defun ar-blok-doublebackticked-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around doublebackticked.
  Returns blok or nil if no DOUBLEBACKTICKED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'doublebackticked no-delimiters))

(defun ar-backslashparen-doublebackticked-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doublebackticked at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'doublebackticked no-delimiters))

(defun ar-doublebackslash-doublebackticked-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'doublebackticked no-delimiters))

(defun ar-doubleslash-doublebackticked-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'doublebackticked no-delimiters))

(defun ar-doublebackslashparen-doublebackticked-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'doublebackticked no-delimiters))

(defun ar-doublebacktick-doublebackticked-atpt (&optional no-delimiters)
  "Provides double backticks around DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'doublebackticked no-delimiters))

(defun ar-slashparen-doublebackticked-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doublebackticked no-delimiters))

(defun ar-slashparen-doublebackticked-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doublebackticked no-delimiters))

(defun ar-comment-doublebackticked-atpt (&optional no-delimiters)
  "Comments DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-comment 'doublebackticked no-delimiters))

(defun ar-commatize-doublebackticked-atpt (&optional no-delimiters)
  "Put a comma after DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'doublebackticked no-delimiters))

(defun ar-quote-doublebackticked-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-quote 'doublebackticked no-delimiters))


(defun ar-mark-doublebackticked-atpt (&optional no-delimiters)
  "Marks DOUBLEBACKTICKED at point if any. "
  (interactive "p")
  (ar-th-mark 'doublebackticked))

(defun ar-hide-doublebackticked-atpt (&optional no-delimiters)
  "Hides DOUBLEBACKTICKED at point. "
  (interactive "p")
  (ar-th-hide 'doublebackticked))

(defun ar-show-doublebackticked-atpt (&optional no-delimiters)
  "Shows hidden DOUBLEBACKTICKED at point. "
  (interactive "p")
  (ar-th-show 'doublebackticked))

(defun ar-hide-show-doublebackticked-atpt (&optional no-delimiters)
  "Alternatively hides or shows DOUBLEBACKTICKED at point. "
  (interactive "p")
  (ar-th-hide-show 'doublebackticked))

(defun ar-highlight-doublebackticked-atpt-mode (&optional no-delimiters)
  "Toggles doublebackticked-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'doublebackticked no-delimiters))

(defun ar-kill-doublebackticked-atpt (&optional no-delimiters)
  "Kills DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-kill 'doublebackticked no-delimiters))

(defun ar-curvedsinglequote-doublebackticked-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'doublebackticked no-delimiters))

(defun ar-separate-doublebackticked-atpt (&optional no-delimiters)
  "Separates DOUBLEBACKTICKED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'doublebackticked no-delimiters))

(defun ar-triplequotedq-doublebackticked-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doublebackticked. "
  (interactive "*P")
  (ar-th-triplequotedq 'doublebackticked no-delimiters))

(defun ar-triplequotesq-doublebackticked-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doublebackticked. "
  (interactive "*P")
  (ar-th-triplequotesq 'doublebackticked no-delimiters))

(defun ar-triplebacktick-doublebackticked-atpt (&optional no-delimiters)
  "Deletes doublebackticked at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'doublebackticked no-delimiters))

(defun ar-trim-doublebackticked-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackticked no-delimiters t t))

(defun ar-left-trim-doublebackticked-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doublebackticked no-delimiters t))

(defun ar-right-trim-doublebackticked-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackticked n no-delimiters nil t))

(defun ar-underscore-doublebackticked-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLEBACKTICKED. "
  (interactive "*P")
  (ar-th-underscore 'doublebackticked no-delimiters))

(defun ar-forward-doublebackticked-atpt (&optional no-delimiters)
  "Moves forward over DOUBLEBACKTICKED at point if any, does nothing otherwise.
Returns end position of DOUBLEBACKTICKED "
  (interactive "p")
  (ar-th-forward 'doublebackticked no-delimiters))

(defun ar-backward-doublebackticked-atpt (&optional no-delimiters)
  "Moves backward over DOUBLEBACKTICKED before point if any, does nothing otherwise.
Returns beginning position of DOUBLEBACKTICKED "
  (interactive "p")
  (ar-th-backward 'doublebackticked no-delimiters))

(defun ar-transpose-doublebackticked-atpt (&optional no-delimiters)
  "Transposes DOUBLEBACKTICKED with DOUBLEBACKTICKED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'doublebackticked no-delimiters))

(defun ar-sort-doublebackticked-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts doublebacktickeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'doublebackticked reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-doublebackticked-atpt (&optional arg)
  "Return t if a DOUBLEBACKTICKED at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-doublebackticked-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doublebackticked-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-doublebackslashedparen-atpt (&optional no-delimiters)
  "Returns doublebackslashedparen at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'doublebackslashedparen no-delimiters))

(defun ar-bounds-of-doublebackslashedparen-atpt (&optional no-delimiters)
  "Returns a list, borders of doublebackslashedparen if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'doublebackslashedparen no-delimiters))

(defun ar-doublebackslashedparen-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'doublebackslashedparen no-delimiters))

(defun ar-doublebackslashedparen-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'doublebackslashedparen no-delimiters))

(defun ar-beginning-of-doublebackslashedparen-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'doublebackslashedparen no-delimiters))

(defun ar-end-of-doublebackslashedparen-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'doublebackslashedparen no-delimiters))

(defun ar-in-doublebackslashedparen-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLEBACKSLASHEDPAREN at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'doublebackslashedparen no-delimiters))

(defun ar-length-of-doublebackslashedparen-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'doublebackslashedparen no-delimiters))

(defun ar-copy-doublebackslashedparen-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'doublebackslashedparen no-delimiters))

(defun ar-delete-doublebackslashedparen-atpt (&optional no-delimiters)
  "Deletes DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-delete 'doublebackslashedparen no-delimiters))

(defun ar-delete-doublebackslashedparen-in-region (beg end)
  "Deletes DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doublebackslashedparen beg end))

(defun ar-blok-doublebackslashedparen-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around doublebackslashedparen.
  Returns blok or nil if no DOUBLEBACKSLASHEDPAREN at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'doublebackslashedparen no-delimiters))

(defun ar-backslashparen-doublebackslashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doublebackslashedparen at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'doublebackslashedparen no-delimiters))

(defun ar-doublebackslash-doublebackslashedparen-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'doublebackslashedparen no-delimiters))

(defun ar-doubleslash-doublebackslashedparen-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'doublebackslashedparen no-delimiters))

(defun ar-doublebackslashparen-doublebackslashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'doublebackslashedparen no-delimiters))

(defun ar-doublebacktick-doublebackslashedparen-atpt (&optional no-delimiters)
  "Provides double backticks around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'doublebackslashedparen no-delimiters))

(defun ar-slashparen-doublebackslashedparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doublebackslashedparen no-delimiters))

(defun ar-slashparen-doublebackslashedparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doublebackslashedparen no-delimiters))

(defun ar-comment-doublebackslashedparen-atpt (&optional no-delimiters)
  "Comments DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-comment 'doublebackslashedparen no-delimiters))

(defun ar-commatize-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put a comma after DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-commatize 'doublebackslashedparen no-delimiters))

(defun ar-quote-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-quote 'doublebackslashedparen no-delimiters))


(defun ar-mark-doublebackslashedparen-atpt (&optional no-delimiters)
  "Marks DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "p")
  (ar-th-mark 'doublebackslashedparen))

(defun ar-hide-doublebackslashedparen-atpt (&optional no-delimiters)
  "Hides DOUBLEBACKSLASHEDPAREN at point. "
  (interactive "p")
  (ar-th-hide 'doublebackslashedparen))

(defun ar-show-doublebackslashedparen-atpt (&optional no-delimiters)
  "Shows hidden DOUBLEBACKSLASHEDPAREN at point. "
  (interactive "p")
  (ar-th-show 'doublebackslashedparen))

(defun ar-hide-show-doublebackslashedparen-atpt (&optional no-delimiters)
  "Alternatively hides or shows DOUBLEBACKSLASHEDPAREN at point. "
  (interactive "p")
  (ar-th-hide-show 'doublebackslashedparen))

(defun ar-highlight-doublebackslashedparen-atpt-mode (&optional no-delimiters)
  "Toggles doublebackslashedparen-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'doublebackslashedparen no-delimiters))

(defun ar-kill-doublebackslashedparen-atpt (&optional no-delimiters)
  "Kills DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-kill 'doublebackslashedparen no-delimiters))

(defun ar-curvedsinglequote-doublebackslashedparen-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'doublebackslashedparen no-delimiters))

(defun ar-separate-doublebackslashedparen-atpt (&optional no-delimiters)
  "Separates DOUBLEBACKSLASHEDPAREN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'doublebackslashedparen no-delimiters))

(defun ar-triplequotedq-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doublebackslashedparen. "
  (interactive "*P")
  (ar-th-triplequotedq 'doublebackslashedparen no-delimiters))

(defun ar-triplequotesq-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doublebackslashedparen. "
  (interactive "*P")
  (ar-th-triplequotesq 'doublebackslashedparen no-delimiters))

(defun ar-triplebacktick-doublebackslashedparen-atpt (&optional no-delimiters)
  "Deletes doublebackslashedparen at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'doublebackslashedparen no-delimiters))

(defun ar-trim-doublebackslashedparen-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashedparen no-delimiters t t))

(defun ar-left-trim-doublebackslashedparen-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashedparen no-delimiters t))

(defun ar-right-trim-doublebackslashedparen-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashedparen n no-delimiters nil t))

(defun ar-underscore-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLEBACKSLASHEDPAREN. "
  (interactive "*P")
  (ar-th-underscore 'doublebackslashedparen no-delimiters))

(defun ar-forward-doublebackslashedparen-atpt (&optional no-delimiters)
  "Moves forward over DOUBLEBACKSLASHEDPAREN at point if any, does nothing otherwise.
Returns end position of DOUBLEBACKSLASHEDPAREN "
  (interactive "p")
  (ar-th-forward 'doublebackslashedparen no-delimiters))

(defun ar-backward-doublebackslashedparen-atpt (&optional no-delimiters)
  "Moves backward over DOUBLEBACKSLASHEDPAREN before point if any, does nothing otherwise.
Returns beginning position of DOUBLEBACKSLASHEDPAREN "
  (interactive "p")
  (ar-th-backward 'doublebackslashedparen no-delimiters))

(defun ar-transpose-doublebackslashedparen-atpt (&optional no-delimiters)
  "Transposes DOUBLEBACKSLASHEDPAREN with DOUBLEBACKSLASHEDPAREN before point if any. "
  (interactive "*P")
  (ar-th-transpose 'doublebackslashedparen no-delimiters))

(defun ar-sort-doublebackslashedparen-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts doublebackslashedparens in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'doublebackslashedparen reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-doublebackslashedparen-atpt (&optional arg)
  "Return t if a DOUBLEBACKSLASHEDPAREN at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-doublebackslashedparen-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doublebackslashedparen-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-doubleslashed-atpt (&optional no-delimiters)
  "Returns doubleslashed at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'doubleslashed no-delimiters))

(defun ar-bounds-of-doubleslashed-atpt (&optional no-delimiters)
  "Returns a list, borders of doubleslashed if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'doubleslashed no-delimiters))

(defun ar-doubleslashed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLESLASHED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'doubleslashed no-delimiters))

(defun ar-doubleslashed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLESLASHED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'doubleslashed no-delimiters))

(defun ar-beginning-of-doubleslashed-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DOUBLESLASHED at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'doubleslashed no-delimiters))

(defun ar-end-of-doubleslashed-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DOUBLESLASHED at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'doubleslashed no-delimiters))

(defun ar-in-doubleslashed-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLESLASHED at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'doubleslashed no-delimiters))

(defun ar-length-of-doubleslashed-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DOUBLESLASHED at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'doubleslashed no-delimiters))

(defun ar-copy-doubleslashed-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLESLASHED at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'doubleslashed no-delimiters))

(defun ar-delete-doubleslashed-atpt (&optional no-delimiters)
  "Deletes DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-delete 'doubleslashed no-delimiters))

(defun ar-delete-doubleslashed-in-region (beg end)
  "Deletes DOUBLESLASHED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doubleslashed beg end))

(defun ar-blok-doubleslashed-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around doubleslashed.
  Returns blok or nil if no DOUBLESLASHED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'doubleslashed no-delimiters))

(defun ar-backslashparen-doubleslashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doubleslashed at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'doubleslashed no-delimiters))

(defun ar-doublebackslash-doubleslashed-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'doubleslashed no-delimiters))

(defun ar-doubleslash-doubleslashed-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'doubleslashed no-delimiters))

(defun ar-doublebackslashparen-doubleslashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'doubleslashed no-delimiters))

(defun ar-doublebacktick-doubleslashed-atpt (&optional no-delimiters)
  "Provides double backticks around DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'doubleslashed no-delimiters))

(defun ar-slashparen-doubleslashed-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doubleslashed no-delimiters))

(defun ar-slashparen-doubleslashed-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doubleslashed no-delimiters))

(defun ar-comment-doubleslashed-atpt (&optional no-delimiters)
  "Comments DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-comment 'doubleslashed no-delimiters))

(defun ar-commatize-doubleslashed-atpt (&optional no-delimiters)
  "Put a comma after DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'doubleslashed no-delimiters))

(defun ar-quote-doubleslashed-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-quote 'doubleslashed no-delimiters))


(defun ar-mark-doubleslashed-atpt (&optional no-delimiters)
  "Marks DOUBLESLASHED at point if any. "
  (interactive "p")
  (ar-th-mark 'doubleslashed))

(defun ar-hide-doubleslashed-atpt (&optional no-delimiters)
  "Hides DOUBLESLASHED at point. "
  (interactive "p")
  (ar-th-hide 'doubleslashed))

(defun ar-show-doubleslashed-atpt (&optional no-delimiters)
  "Shows hidden DOUBLESLASHED at point. "
  (interactive "p")
  (ar-th-show 'doubleslashed))

(defun ar-hide-show-doubleslashed-atpt (&optional no-delimiters)
  "Alternatively hides or shows DOUBLESLASHED at point. "
  (interactive "p")
  (ar-th-hide-show 'doubleslashed))

(defun ar-highlight-doubleslashed-atpt-mode (&optional no-delimiters)
  "Toggles doubleslashed-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'doubleslashed no-delimiters))

(defun ar-kill-doubleslashed-atpt (&optional no-delimiters)
  "Kills DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-kill 'doubleslashed no-delimiters))

(defun ar-curvedsinglequote-doubleslashed-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'doubleslashed no-delimiters))

(defun ar-separate-doubleslashed-atpt (&optional no-delimiters)
  "Separates DOUBLESLASHED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'doubleslashed no-delimiters))

(defun ar-triplequotedq-doubleslashed-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doubleslashed. "
  (interactive "*P")
  (ar-th-triplequotedq 'doubleslashed no-delimiters))

(defun ar-triplequotesq-doubleslashed-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doubleslashed. "
  (interactive "*P")
  (ar-th-triplequotesq 'doubleslashed no-delimiters))

(defun ar-triplebacktick-doubleslashed-atpt (&optional no-delimiters)
  "Deletes doubleslashed at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'doubleslashed no-delimiters))

(defun ar-trim-doubleslashed-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doubleslashed no-delimiters t t))

(defun ar-left-trim-doubleslashed-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doubleslashed no-delimiters t))

(defun ar-right-trim-doubleslashed-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doubleslashed n no-delimiters nil t))

(defun ar-underscore-doubleslashed-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLESLASHED. "
  (interactive "*P")
  (ar-th-underscore 'doubleslashed no-delimiters))

(defun ar-forward-doubleslashed-atpt (&optional no-delimiters)
  "Moves forward over DOUBLESLASHED at point if any, does nothing otherwise.
Returns end position of DOUBLESLASHED "
  (interactive "p")
  (ar-th-forward 'doubleslashed no-delimiters))

(defun ar-backward-doubleslashed-atpt (&optional no-delimiters)
  "Moves backward over DOUBLESLASHED before point if any, does nothing otherwise.
Returns beginning position of DOUBLESLASHED "
  (interactive "p")
  (ar-th-backward 'doubleslashed no-delimiters))

(defun ar-transpose-doubleslashed-atpt (&optional no-delimiters)
  "Transposes DOUBLESLASHED with DOUBLESLASHED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'doubleslashed no-delimiters))

(defun ar-sort-doubleslashed-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts doubleslasheds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'doubleslashed reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-doubleslashed-atpt (&optional arg)
  "Return t if a DOUBLESLASHED at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-doubleslashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doubleslashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-doubleslashedparen-atpt (&optional no-delimiters)
  "Returns doubleslashedparen at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'doubleslashedparen no-delimiters))

(defun ar-bounds-of-doubleslashedparen-atpt (&optional no-delimiters)
  "Returns a list, borders of doubleslashedparen if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'doubleslashedparen no-delimiters))

(defun ar-doubleslashedparen-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLESLASHEDPAREN at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'doubleslashedparen no-delimiters))

(defun ar-doubleslashedparen-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLESLASHEDPAREN at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'doubleslashedparen no-delimiters))

(defun ar-beginning-of-doubleslashedparen-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DOUBLESLASHEDPAREN at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'doubleslashedparen no-delimiters))

(defun ar-end-of-doubleslashedparen-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DOUBLESLASHEDPAREN at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'doubleslashedparen no-delimiters))

(defun ar-in-doubleslashedparen-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLESLASHEDPAREN at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'doubleslashedparen no-delimiters))

(defun ar-length-of-doubleslashedparen-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DOUBLESLASHEDPAREN at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'doubleslashedparen no-delimiters))

(defun ar-copy-doubleslashedparen-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLESLASHEDPAREN at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'doubleslashedparen no-delimiters))

(defun ar-delete-doubleslashedparen-atpt (&optional no-delimiters)
  "Deletes DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-delete 'doubleslashedparen no-delimiters))

(defun ar-delete-doubleslashedparen-in-region (beg end)
  "Deletes DOUBLESLASHEDPAREN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doubleslashedparen beg end))

(defun ar-blok-doubleslashedparen-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around doubleslashedparen.
  Returns blok or nil if no DOUBLESLASHEDPAREN at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'doubleslashedparen no-delimiters))

(defun ar-backslashparen-doubleslashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doubleslashedparen at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'doubleslashedparen no-delimiters))

(defun ar-doublebackslash-doubleslashedparen-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'doubleslashedparen no-delimiters))

(defun ar-doubleslash-doubleslashedparen-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'doubleslashedparen no-delimiters))

(defun ar-doublebackslashparen-doubleslashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'doubleslashedparen no-delimiters))

(defun ar-doublebacktick-doubleslashedparen-atpt (&optional no-delimiters)
  "Provides double backticks around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'doubleslashedparen no-delimiters))

(defun ar-slashparen-doubleslashedparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doubleslashedparen no-delimiters))

(defun ar-slashparen-doubleslashedparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doubleslashedparen no-delimiters))

(defun ar-comment-doubleslashedparen-atpt (&optional no-delimiters)
  "Comments DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-comment 'doubleslashedparen no-delimiters))

(defun ar-commatize-doubleslashedparen-atpt (&optional no-delimiters)
  "Put a comma after DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-commatize 'doubleslashedparen no-delimiters))

(defun ar-quote-doubleslashedparen-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-quote 'doubleslashedparen no-delimiters))


(defun ar-mark-doubleslashedparen-atpt (&optional no-delimiters)
  "Marks DOUBLESLASHEDPAREN at point if any. "
  (interactive "p")
  (ar-th-mark 'doubleslashedparen))

(defun ar-hide-doubleslashedparen-atpt (&optional no-delimiters)
  "Hides DOUBLESLASHEDPAREN at point. "
  (interactive "p")
  (ar-th-hide 'doubleslashedparen))

(defun ar-show-doubleslashedparen-atpt (&optional no-delimiters)
  "Shows hidden DOUBLESLASHEDPAREN at point. "
  (interactive "p")
  (ar-th-show 'doubleslashedparen))

(defun ar-hide-show-doubleslashedparen-atpt (&optional no-delimiters)
  "Alternatively hides or shows DOUBLESLASHEDPAREN at point. "
  (interactive "p")
  (ar-th-hide-show 'doubleslashedparen))

(defun ar-highlight-doubleslashedparen-atpt-mode (&optional no-delimiters)
  "Toggles doubleslashedparen-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'doubleslashedparen no-delimiters))

(defun ar-kill-doubleslashedparen-atpt (&optional no-delimiters)
  "Kills DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-kill 'doubleslashedparen no-delimiters))

(defun ar-curvedsinglequote-doubleslashedparen-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'doubleslashedparen no-delimiters))

(defun ar-separate-doubleslashedparen-atpt (&optional no-delimiters)
  "Separates DOUBLESLASHEDPAREN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'doubleslashedparen no-delimiters))

(defun ar-triplequotedq-doubleslashedparen-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doubleslashedparen. "
  (interactive "*P")
  (ar-th-triplequotedq 'doubleslashedparen no-delimiters))

(defun ar-triplequotesq-doubleslashedparen-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doubleslashedparen. "
  (interactive "*P")
  (ar-th-triplequotesq 'doubleslashedparen no-delimiters))

(defun ar-triplebacktick-doubleslashedparen-atpt (&optional no-delimiters)
  "Deletes doubleslashedparen at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'doubleslashedparen no-delimiters))

(defun ar-trim-doubleslashedparen-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doubleslashedparen no-delimiters t t))

(defun ar-left-trim-doubleslashedparen-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doubleslashedparen no-delimiters t))

(defun ar-right-trim-doubleslashedparen-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doubleslashedparen n no-delimiters nil t))

(defun ar-underscore-doubleslashedparen-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLESLASHEDPAREN. "
  (interactive "*P")
  (ar-th-underscore 'doubleslashedparen no-delimiters))

(defun ar-forward-doubleslashedparen-atpt (&optional no-delimiters)
  "Moves forward over DOUBLESLASHEDPAREN at point if any, does nothing otherwise.
Returns end position of DOUBLESLASHEDPAREN "
  (interactive "p")
  (ar-th-forward 'doubleslashedparen no-delimiters))

(defun ar-backward-doubleslashedparen-atpt (&optional no-delimiters)
  "Moves backward over DOUBLESLASHEDPAREN before point if any, does nothing otherwise.
Returns beginning position of DOUBLESLASHEDPAREN "
  (interactive "p")
  (ar-th-backward 'doubleslashedparen no-delimiters))

(defun ar-transpose-doubleslashedparen-atpt (&optional no-delimiters)
  "Transposes DOUBLESLASHEDPAREN with DOUBLESLASHEDPAREN before point if any. "
  (interactive "*P")
  (ar-th-transpose 'doubleslashedparen no-delimiters))

(defun ar-sort-doubleslashedparen-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts doubleslashedparens in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'doubleslashedparen reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-doubleslashedparen-atpt (&optional arg)
  "Return t if a DOUBLESLASHEDPAREN at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-doubleslashedparen-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doubleslashedparen-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-markup-atpt (&optional no-delimiters)
  "Returns markup at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'markup no-delimiters))

(defun ar-bounds-of-markup-atpt (&optional no-delimiters)
  "Returns a list, borders of markup if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'markup no-delimiters))

(defun ar-markup-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position MARKUP at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'markup no-delimiters))

(defun ar-markup-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of MARKUP at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'markup no-delimiters))

(defun ar-beginning-of-markup-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class MARKUP at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'markup no-delimiters))

(defun ar-end-of-markup-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class MARKUP at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'markup no-delimiters))

(defun ar-in-markup-p-atpt (&optional no-delimiters)
  "Returns bounds of MARKUP at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'markup no-delimiters))

(defun ar-length-of-markup-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class MARKUP at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'markup no-delimiters))

(defun ar-copy-markup-atpt (&optional no-delimiters)
  "Returns a copy of MARKUP at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'markup no-delimiters))

(defun ar-delete-markup-atpt (&optional no-delimiters)
  "Deletes MARKUP at point if any. "
  (interactive "*P")
  (ar-th-delete 'markup no-delimiters))

(defun ar-delete-markup-in-region (beg end)
  "Deletes MARKUP at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'markup beg end))

(defun ar-blok-markup-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around markup.
  Returns blok or nil if no MARKUP at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'markup no-delimiters))

(defun ar-backslashparen-markup-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around markup at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'markup no-delimiters))

(defun ar-doublebackslash-markup-atpt (&optional no-delimiters)
  "Puts doubled backslashes around MARKUP at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'markup no-delimiters))

(defun ar-doubleslash-markup-atpt (&optional no-delimiters)
  "Puts doubled slashes around MARKUP at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'markup no-delimiters))

(defun ar-doublebackslashparen-markup-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around MARKUP at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'markup no-delimiters))

(defun ar-doublebacktick-markup-atpt (&optional no-delimiters)
  "Provides double backticks around MARKUP at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'markup no-delimiters))

(defun ar-slashparen-markup-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MARKUP at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'markup no-delimiters))

(defun ar-slashparen-markup-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MARKUP at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'markup no-delimiters))

(defun ar-comment-markup-atpt (&optional no-delimiters)
  "Comments MARKUP at point if any. "
  (interactive "*P")
  (ar-th-comment 'markup no-delimiters))

(defun ar-commatize-markup-atpt (&optional no-delimiters)
  "Put a comma after MARKUP at point if any. "
  (interactive "*P")
  (ar-th-commatize 'markup no-delimiters))

(defun ar-quote-markup-atpt (&optional no-delimiters)
  "Put a singlequote before MARKUP at point if any. "
  (interactive "*P")
  (ar-th-quote 'markup no-delimiters))


(defun ar-mark-markup-atpt (&optional no-delimiters)
  "Marks MARKUP at point if any. "
  (interactive "p")
  (ar-th-mark 'markup))

(defun ar-hide-markup-atpt (&optional no-delimiters)
  "Hides MARKUP at point. "
  (interactive "p")
  (ar-th-hide 'markup))

(defun ar-show-markup-atpt (&optional no-delimiters)
  "Shows hidden MARKUP at point. "
  (interactive "p")
  (ar-th-show 'markup))

(defun ar-hide-show-markup-atpt (&optional no-delimiters)
  "Alternatively hides or shows MARKUP at point. "
  (interactive "p")
  (ar-th-hide-show 'markup))

(defun ar-highlight-markup-atpt-mode (&optional no-delimiters)
  "Toggles markup-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'markup no-delimiters))

(defun ar-kill-markup-atpt (&optional no-delimiters)
  "Kills MARKUP at point if any. "
  (interactive "*P")
  (ar-th-kill 'markup no-delimiters))

(defun ar-curvedsinglequote-markup-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'markup no-delimiters))

(defun ar-separate-markup-atpt (&optional no-delimiters)
  "Separates MARKUP at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'markup no-delimiters))

(defun ar-triplequotedq-markup-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around markup. "
  (interactive "*P")
  (ar-th-triplequotedq 'markup no-delimiters))

(defun ar-triplequotesq-markup-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around markup. "
  (interactive "*P")
  (ar-th-triplequotesq 'markup no-delimiters))

(defun ar-triplebacktick-markup-atpt (&optional no-delimiters)
  "Deletes markup at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'markup no-delimiters))

(defun ar-trim-markup-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'markup no-delimiters t t))

(defun ar-left-trim-markup-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'markup no-delimiters t))

(defun ar-right-trim-markup-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'markup n no-delimiters nil t))

(defun ar-underscore-markup-atpt (&optional no-delimiters)
  "Put underscore char around MARKUP. "
  (interactive "*P")
  (ar-th-underscore 'markup no-delimiters))

(defun ar-forward-markup-atpt (&optional no-delimiters)
  "Moves forward over MARKUP at point if any, does nothing otherwise.
Returns end position of MARKUP "
  (interactive "p")
  (ar-th-forward 'markup no-delimiters))

(defun ar-backward-markup-atpt (&optional no-delimiters)
  "Moves backward over MARKUP before point if any, does nothing otherwise.
Returns beginning position of MARKUP "
  (interactive "p")
  (ar-th-backward 'markup no-delimiters))

(defun ar-transpose-markup-atpt (&optional no-delimiters)
  "Transposes MARKUP with MARKUP before point if any. "
  (interactive "*P")
  (ar-th-transpose 'markup no-delimiters))

(defun ar-sort-markup-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts markups in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'markup reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-markup-atpt (&optional arg)
  "Return t if a MARKUP at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-markup-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-markup-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-mldata-atpt (&optional no-delimiters)
  "Returns mldata at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'mldata no-delimiters))

(defun ar-bounds-of-mldata-atpt (&optional no-delimiters)
  "Returns a list, borders of mldata if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'mldata no-delimiters))

(defun ar-mldata-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position MLDATA at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'mldata no-delimiters))

(defun ar-mldata-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of MLDATA at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'mldata no-delimiters))

(defun ar-beginning-of-mldata-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class MLDATA at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'mldata no-delimiters))

(defun ar-end-of-mldata-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class MLDATA at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'mldata no-delimiters))

(defun ar-in-mldata-p-atpt (&optional no-delimiters)
  "Returns bounds of MLDATA at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'mldata no-delimiters))

(defun ar-length-of-mldata-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class MLDATA at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'mldata no-delimiters))

(defun ar-copy-mldata-atpt (&optional no-delimiters)
  "Returns a copy of MLDATA at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'mldata no-delimiters))

(defun ar-delete-mldata-atpt (&optional no-delimiters)
  "Deletes MLDATA at point if any. "
  (interactive "*P")
  (ar-th-delete 'mldata no-delimiters))

(defun ar-delete-mldata-in-region (beg end)
  "Deletes MLDATA at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'mldata beg end))

(defun ar-blok-mldata-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around mldata.
  Returns blok or nil if no MLDATA at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'mldata no-delimiters))

(defun ar-backslashparen-mldata-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around mldata at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'mldata no-delimiters))

(defun ar-doublebackslash-mldata-atpt (&optional no-delimiters)
  "Puts doubled backslashes around MLDATA at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'mldata no-delimiters))

(defun ar-doubleslash-mldata-atpt (&optional no-delimiters)
  "Puts doubled slashes around MLDATA at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'mldata no-delimiters))

(defun ar-doublebackslashparen-mldata-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around MLDATA at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'mldata no-delimiters))

(defun ar-doublebacktick-mldata-atpt (&optional no-delimiters)
  "Provides double backticks around MLDATA at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'mldata no-delimiters))

(defun ar-slashparen-mldata-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MLDATA at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'mldata no-delimiters))

(defun ar-slashparen-mldata-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MLDATA at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'mldata no-delimiters))

(defun ar-comment-mldata-atpt (&optional no-delimiters)
  "Comments MLDATA at point if any. "
  (interactive "*P")
  (ar-th-comment 'mldata no-delimiters))

(defun ar-commatize-mldata-atpt (&optional no-delimiters)
  "Put a comma after MLDATA at point if any. "
  (interactive "*P")
  (ar-th-commatize 'mldata no-delimiters))

(defun ar-quote-mldata-atpt (&optional no-delimiters)
  "Put a singlequote before MLDATA at point if any. "
  (interactive "*P")
  (ar-th-quote 'mldata no-delimiters))


(defun ar-mark-mldata-atpt (&optional no-delimiters)
  "Marks MLDATA at point if any. "
  (interactive "p")
  (ar-th-mark 'mldata))

(defun ar-hide-mldata-atpt (&optional no-delimiters)
  "Hides MLDATA at point. "
  (interactive "p")
  (ar-th-hide 'mldata))

(defun ar-show-mldata-atpt (&optional no-delimiters)
  "Shows hidden MLDATA at point. "
  (interactive "p")
  (ar-th-show 'mldata))

(defun ar-hide-show-mldata-atpt (&optional no-delimiters)
  "Alternatively hides or shows MLDATA at point. "
  (interactive "p")
  (ar-th-hide-show 'mldata))

(defun ar-highlight-mldata-atpt-mode (&optional no-delimiters)
  "Toggles mldata-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'mldata no-delimiters))

(defun ar-kill-mldata-atpt (&optional no-delimiters)
  "Kills MLDATA at point if any. "
  (interactive "*P")
  (ar-th-kill 'mldata no-delimiters))

(defun ar-curvedsinglequote-mldata-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'mldata no-delimiters))

(defun ar-separate-mldata-atpt (&optional no-delimiters)
  "Separates MLDATA at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'mldata no-delimiters))

(defun ar-triplequotedq-mldata-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around mldata. "
  (interactive "*P")
  (ar-th-triplequotedq 'mldata no-delimiters))

(defun ar-triplequotesq-mldata-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around mldata. "
  (interactive "*P")
  (ar-th-triplequotesq 'mldata no-delimiters))

(defun ar-triplebacktick-mldata-atpt (&optional no-delimiters)
  "Deletes mldata at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'mldata no-delimiters))

(defun ar-trim-mldata-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'mldata no-delimiters t t))

(defun ar-left-trim-mldata-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'mldata no-delimiters t))

(defun ar-right-trim-mldata-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'mldata n no-delimiters nil t))

(defun ar-underscore-mldata-atpt (&optional no-delimiters)
  "Put underscore char around MLDATA. "
  (interactive "*P")
  (ar-th-underscore 'mldata no-delimiters))

(defun ar-forward-mldata-atpt (&optional no-delimiters)
  "Moves forward over MLDATA at point if any, does nothing otherwise.
Returns end position of MLDATA "
  (interactive "p")
  (ar-th-forward 'mldata no-delimiters))

(defun ar-backward-mldata-atpt (&optional no-delimiters)
  "Moves backward over MLDATA before point if any, does nothing otherwise.
Returns beginning position of MLDATA "
  (interactive "p")
  (ar-th-backward 'mldata no-delimiters))

(defun ar-transpose-mldata-atpt (&optional no-delimiters)
  "Transposes MLDATA with MLDATA before point if any. "
  (interactive "*P")
  (ar-th-transpose 'mldata no-delimiters))

(defun ar-sort-mldata-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts mldatas in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'mldata reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-mldata-atpt (&optional arg)
  "Return t if a MLDATA at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-mldata-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-mldata-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-mlattribut-atpt (&optional no-delimiters)
  "Returns mlattribut at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'mlattribut no-delimiters))

(defun ar-bounds-of-mlattribut-atpt (&optional no-delimiters)
  "Returns a list, borders of mlattribut if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'mlattribut no-delimiters))

(defun ar-mlattribut-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position MLATTRIBUT at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'mlattribut no-delimiters))

(defun ar-mlattribut-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of MLATTRIBUT at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'mlattribut no-delimiters))

(defun ar-beginning-of-mlattribut-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class MLATTRIBUT at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'mlattribut no-delimiters))

(defun ar-end-of-mlattribut-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class MLATTRIBUT at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'mlattribut no-delimiters))

(defun ar-in-mlattribut-p-atpt (&optional no-delimiters)
  "Returns bounds of MLATTRIBUT at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'mlattribut no-delimiters))

(defun ar-length-of-mlattribut-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class MLATTRIBUT at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'mlattribut no-delimiters))

(defun ar-copy-mlattribut-atpt (&optional no-delimiters)
  "Returns a copy of MLATTRIBUT at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'mlattribut no-delimiters))

(defun ar-delete-mlattribut-atpt (&optional no-delimiters)
  "Deletes MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-delete 'mlattribut no-delimiters))

(defun ar-delete-mlattribut-in-region (beg end)
  "Deletes MLATTRIBUT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'mlattribut beg end))

(defun ar-blok-mlattribut-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around mlattribut.
  Returns blok or nil if no MLATTRIBUT at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'mlattribut no-delimiters))

(defun ar-backslashparen-mlattribut-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around mlattribut at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'mlattribut no-delimiters))

(defun ar-doublebackslash-mlattribut-atpt (&optional no-delimiters)
  "Puts doubled backslashes around MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'mlattribut no-delimiters))

(defun ar-doubleslash-mlattribut-atpt (&optional no-delimiters)
  "Puts doubled slashes around MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'mlattribut no-delimiters))

(defun ar-doublebackslashparen-mlattribut-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'mlattribut no-delimiters))

(defun ar-doublebacktick-mlattribut-atpt (&optional no-delimiters)
  "Provides double backticks around MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'mlattribut no-delimiters))

(defun ar-slashparen-mlattribut-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'mlattribut no-delimiters))

(defun ar-slashparen-mlattribut-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'mlattribut no-delimiters))

(defun ar-comment-mlattribut-atpt (&optional no-delimiters)
  "Comments MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-comment 'mlattribut no-delimiters))

(defun ar-commatize-mlattribut-atpt (&optional no-delimiters)
  "Put a comma after MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-commatize 'mlattribut no-delimiters))

(defun ar-quote-mlattribut-atpt (&optional no-delimiters)
  "Put a singlequote before MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-quote 'mlattribut no-delimiters))


(defun ar-mark-mlattribut-atpt (&optional no-delimiters)
  "Marks MLATTRIBUT at point if any. "
  (interactive "p")
  (ar-th-mark 'mlattribut))

(defun ar-hide-mlattribut-atpt (&optional no-delimiters)
  "Hides MLATTRIBUT at point. "
  (interactive "p")
  (ar-th-hide 'mlattribut))

(defun ar-show-mlattribut-atpt (&optional no-delimiters)
  "Shows hidden MLATTRIBUT at point. "
  (interactive "p")
  (ar-th-show 'mlattribut))

(defun ar-hide-show-mlattribut-atpt (&optional no-delimiters)
  "Alternatively hides or shows MLATTRIBUT at point. "
  (interactive "p")
  (ar-th-hide-show 'mlattribut))

(defun ar-highlight-mlattribut-atpt-mode (&optional no-delimiters)
  "Toggles mlattribut-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'mlattribut no-delimiters))

(defun ar-kill-mlattribut-atpt (&optional no-delimiters)
  "Kills MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-kill 'mlattribut no-delimiters))

(defun ar-curvedsinglequote-mlattribut-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'mlattribut no-delimiters))

(defun ar-separate-mlattribut-atpt (&optional no-delimiters)
  "Separates MLATTRIBUT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'mlattribut no-delimiters))

(defun ar-triplequotedq-mlattribut-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around mlattribut. "
  (interactive "*P")
  (ar-th-triplequotedq 'mlattribut no-delimiters))

(defun ar-triplequotesq-mlattribut-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around mlattribut. "
  (interactive "*P")
  (ar-th-triplequotesq 'mlattribut no-delimiters))

(defun ar-triplebacktick-mlattribut-atpt (&optional no-delimiters)
  "Deletes mlattribut at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'mlattribut no-delimiters))

(defun ar-trim-mlattribut-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'mlattribut no-delimiters t t))

(defun ar-left-trim-mlattribut-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'mlattribut no-delimiters t))

(defun ar-right-trim-mlattribut-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'mlattribut n no-delimiters nil t))

(defun ar-underscore-mlattribut-atpt (&optional no-delimiters)
  "Put underscore char around MLATTRIBUT. "
  (interactive "*P")
  (ar-th-underscore 'mlattribut no-delimiters))

(defun ar-forward-mlattribut-atpt (&optional no-delimiters)
  "Moves forward over MLATTRIBUT at point if any, does nothing otherwise.
Returns end position of MLATTRIBUT "
  (interactive "p")
  (ar-th-forward 'mlattribut no-delimiters))

(defun ar-backward-mlattribut-atpt (&optional no-delimiters)
  "Moves backward over MLATTRIBUT before point if any, does nothing otherwise.
Returns beginning position of MLATTRIBUT "
  (interactive "p")
  (ar-th-backward 'mlattribut no-delimiters))

(defun ar-transpose-mlattribut-atpt (&optional no-delimiters)
  "Transposes MLATTRIBUT with MLATTRIBUT before point if any. "
  (interactive "*P")
  (ar-th-transpose 'mlattribut no-delimiters))

(defun ar-sort-mlattribut-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts mlattributs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'mlattribut reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-mlattribut-atpt (&optional arg)
  "Return t if a MLATTRIBUT at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-mlattribut-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-mlattribut-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-mltag-atpt (&optional no-delimiters)
  "Returns mltag at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'mltag no-delimiters))

(defun ar-bounds-of-mltag-atpt (&optional no-delimiters)
  "Returns a list, borders of mltag if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'mltag no-delimiters))

(defun ar-mltag-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position MLTAG at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'mltag no-delimiters))

(defun ar-mltag-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of MLTAG at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'mltag no-delimiters))

(defun ar-beginning-of-mltag-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class MLTAG at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'mltag no-delimiters))

(defun ar-end-of-mltag-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class MLTAG at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'mltag no-delimiters))

(defun ar-in-mltag-p-atpt (&optional no-delimiters)
  "Returns bounds of MLTAG at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'mltag no-delimiters))

(defun ar-length-of-mltag-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class MLTAG at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'mltag no-delimiters))

(defun ar-copy-mltag-atpt (&optional no-delimiters)
  "Returns a copy of MLTAG at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'mltag no-delimiters))

(defun ar-delete-mltag-atpt (&optional no-delimiters)
  "Deletes MLTAG at point if any. "
  (interactive "*P")
  (ar-th-delete 'mltag no-delimiters))

(defun ar-delete-mltag-in-region (beg end)
  "Deletes MLTAG at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'mltag beg end))

(defun ar-blok-mltag-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around mltag.
  Returns blok or nil if no MLTAG at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'mltag no-delimiters))

(defun ar-backslashparen-mltag-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around mltag at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'mltag no-delimiters))

(defun ar-doublebackslash-mltag-atpt (&optional no-delimiters)
  "Puts doubled backslashes around MLTAG at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'mltag no-delimiters))

(defun ar-doubleslash-mltag-atpt (&optional no-delimiters)
  "Puts doubled slashes around MLTAG at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'mltag no-delimiters))

(defun ar-doublebackslashparen-mltag-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around MLTAG at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'mltag no-delimiters))

(defun ar-doublebacktick-mltag-atpt (&optional no-delimiters)
  "Provides double backticks around MLTAG at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'mltag no-delimiters))

(defun ar-slashparen-mltag-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MLTAG at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'mltag no-delimiters))

(defun ar-slashparen-mltag-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MLTAG at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'mltag no-delimiters))

(defun ar-comment-mltag-atpt (&optional no-delimiters)
  "Comments MLTAG at point if any. "
  (interactive "*P")
  (ar-th-comment 'mltag no-delimiters))

(defun ar-commatize-mltag-atpt (&optional no-delimiters)
  "Put a comma after MLTAG at point if any. "
  (interactive "*P")
  (ar-th-commatize 'mltag no-delimiters))

(defun ar-quote-mltag-atpt (&optional no-delimiters)
  "Put a singlequote before MLTAG at point if any. "
  (interactive "*P")
  (ar-th-quote 'mltag no-delimiters))


(defun ar-mark-mltag-atpt (&optional no-delimiters)
  "Marks MLTAG at point if any. "
  (interactive "p")
  (ar-th-mark 'mltag))

(defun ar-hide-mltag-atpt (&optional no-delimiters)
  "Hides MLTAG at point. "
  (interactive "p")
  (ar-th-hide 'mltag))

(defun ar-show-mltag-atpt (&optional no-delimiters)
  "Shows hidden MLTAG at point. "
  (interactive "p")
  (ar-th-show 'mltag))

(defun ar-hide-show-mltag-atpt (&optional no-delimiters)
  "Alternatively hides or shows MLTAG at point. "
  (interactive "p")
  (ar-th-hide-show 'mltag))

(defun ar-highlight-mltag-atpt-mode (&optional no-delimiters)
  "Toggles mltag-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'mltag no-delimiters))

(defun ar-kill-mltag-atpt (&optional no-delimiters)
  "Kills MLTAG at point if any. "
  (interactive "*P")
  (ar-th-kill 'mltag no-delimiters))

(defun ar-curvedsinglequote-mltag-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'mltag no-delimiters))

(defun ar-separate-mltag-atpt (&optional no-delimiters)
  "Separates MLTAG at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'mltag no-delimiters))

(defun ar-triplequotedq-mltag-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around mltag. "
  (interactive "*P")
  (ar-th-triplequotedq 'mltag no-delimiters))

(defun ar-triplequotesq-mltag-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around mltag. "
  (interactive "*P")
  (ar-th-triplequotesq 'mltag no-delimiters))

(defun ar-triplebacktick-mltag-atpt (&optional no-delimiters)
  "Deletes mltag at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'mltag no-delimiters))

(defun ar-trim-mltag-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'mltag no-delimiters t t))

(defun ar-left-trim-mltag-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'mltag no-delimiters t))

(defun ar-right-trim-mltag-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'mltag n no-delimiters nil t))

(defun ar-underscore-mltag-atpt (&optional no-delimiters)
  "Put underscore char around MLTAG. "
  (interactive "*P")
  (ar-th-underscore 'mltag no-delimiters))

(defun ar-forward-mltag-atpt (&optional no-delimiters)
  "Moves forward over MLTAG at point if any, does nothing otherwise.
Returns end position of MLTAG "
  (interactive "p")
  (ar-th-forward 'mltag no-delimiters))

(defun ar-backward-mltag-atpt (&optional no-delimiters)
  "Moves backward over MLTAG before point if any, does nothing otherwise.
Returns beginning position of MLTAG "
  (interactive "p")
  (ar-th-backward 'mltag no-delimiters))

(defun ar-transpose-mltag-atpt (&optional no-delimiters)
  "Transposes MLTAG with MLTAG before point if any. "
  (interactive "*P")
  (ar-th-transpose 'mltag no-delimiters))

(defun ar-sort-mltag-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts mltags in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'mltag reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-mltag-atpt (&optional arg)
  "Return t if a MLTAG at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-mltag-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-mltag-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-slashedparen-atpt (&optional no-delimiters)
  "Returns slashedparen at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'slashedparen no-delimiters))

(defun ar-bounds-of-slashedparen-atpt (&optional no-delimiters)
  "Returns a list, borders of slashedparen if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'slashedparen no-delimiters))

(defun ar-slashedparen-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position SLASHEDPAREN at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'slashedparen no-delimiters))

(defun ar-slashedparen-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of SLASHEDPAREN at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'slashedparen no-delimiters))

(defun ar-beginning-of-slashedparen-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class SLASHEDPAREN at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'slashedparen no-delimiters))

(defun ar-end-of-slashedparen-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class SLASHEDPAREN at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'slashedparen no-delimiters))

(defun ar-in-slashedparen-p-atpt (&optional no-delimiters)
  "Returns bounds of SLASHEDPAREN at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'slashedparen no-delimiters))

(defun ar-length-of-slashedparen-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class SLASHEDPAREN at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'slashedparen no-delimiters))

(defun ar-copy-slashedparen-atpt (&optional no-delimiters)
  "Returns a copy of SLASHEDPAREN at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'slashedparen no-delimiters))

(defun ar-delete-slashedparen-atpt (&optional no-delimiters)
  "Deletes SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-delete 'slashedparen no-delimiters))

(defun ar-delete-slashedparen-in-region (beg end)
  "Deletes SLASHEDPAREN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'slashedparen beg end))

(defun ar-blok-slashedparen-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around slashedparen.
  Returns blok or nil if no SLASHEDPAREN at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'slashedparen no-delimiters))

(defun ar-backslashparen-slashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around slashedparen at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'slashedparen no-delimiters))

(defun ar-doublebackslash-slashedparen-atpt (&optional no-delimiters)
  "Puts doubled backslashes around SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'slashedparen no-delimiters))

(defun ar-doubleslash-slashedparen-atpt (&optional no-delimiters)
  "Puts doubled slashes around SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'slashedparen no-delimiters))

(defun ar-doublebackslashparen-slashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'slashedparen no-delimiters))

(defun ar-doublebacktick-slashedparen-atpt (&optional no-delimiters)
  "Provides double backticks around SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'slashedparen no-delimiters))

(defun ar-slashparen-slashedparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'slashedparen no-delimiters))

(defun ar-slashparen-slashedparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'slashedparen no-delimiters))

(defun ar-comment-slashedparen-atpt (&optional no-delimiters)
  "Comments SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-comment 'slashedparen no-delimiters))

(defun ar-commatize-slashedparen-atpt (&optional no-delimiters)
  "Put a comma after SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-commatize 'slashedparen no-delimiters))

(defun ar-quote-slashedparen-atpt (&optional no-delimiters)
  "Put a singlequote before SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-quote 'slashedparen no-delimiters))


(defun ar-mark-slashedparen-atpt (&optional no-delimiters)
  "Marks SLASHEDPAREN at point if any. "
  (interactive "p")
  (ar-th-mark 'slashedparen))

(defun ar-hide-slashedparen-atpt (&optional no-delimiters)
  "Hides SLASHEDPAREN at point. "
  (interactive "p")
  (ar-th-hide 'slashedparen))

(defun ar-show-slashedparen-atpt (&optional no-delimiters)
  "Shows hidden SLASHEDPAREN at point. "
  (interactive "p")
  (ar-th-show 'slashedparen))

(defun ar-hide-show-slashedparen-atpt (&optional no-delimiters)
  "Alternatively hides or shows SLASHEDPAREN at point. "
  (interactive "p")
  (ar-th-hide-show 'slashedparen))

(defun ar-highlight-slashedparen-atpt-mode (&optional no-delimiters)
  "Toggles slashedparen-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'slashedparen no-delimiters))

(defun ar-kill-slashedparen-atpt (&optional no-delimiters)
  "Kills SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-kill 'slashedparen no-delimiters))

(defun ar-curvedsinglequote-slashedparen-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'slashedparen no-delimiters))

(defun ar-separate-slashedparen-atpt (&optional no-delimiters)
  "Separates SLASHEDPAREN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'slashedparen no-delimiters))

(defun ar-triplequotedq-slashedparen-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around slashedparen. "
  (interactive "*P")
  (ar-th-triplequotedq 'slashedparen no-delimiters))

(defun ar-triplequotesq-slashedparen-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around slashedparen. "
  (interactive "*P")
  (ar-th-triplequotesq 'slashedparen no-delimiters))

(defun ar-triplebacktick-slashedparen-atpt (&optional no-delimiters)
  "Deletes slashedparen at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'slashedparen no-delimiters))

(defun ar-trim-slashedparen-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'slashedparen no-delimiters t t))

(defun ar-left-trim-slashedparen-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'slashedparen no-delimiters t))

(defun ar-right-trim-slashedparen-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'slashedparen n no-delimiters nil t))

(defun ar-underscore-slashedparen-atpt (&optional no-delimiters)
  "Put underscore char around SLASHEDPAREN. "
  (interactive "*P")
  (ar-th-underscore 'slashedparen no-delimiters))

(defun ar-forward-slashedparen-atpt (&optional no-delimiters)
  "Moves forward over SLASHEDPAREN at point if any, does nothing otherwise.
Returns end position of SLASHEDPAREN "
  (interactive "p")
  (ar-th-forward 'slashedparen no-delimiters))

(defun ar-backward-slashedparen-atpt (&optional no-delimiters)
  "Moves backward over SLASHEDPAREN before point if any, does nothing otherwise.
Returns beginning position of SLASHEDPAREN "
  (interactive "p")
  (ar-th-backward 'slashedparen no-delimiters))

(defun ar-transpose-slashedparen-atpt (&optional no-delimiters)
  "Transposes SLASHEDPAREN with SLASHEDPAREN before point if any. "
  (interactive "*P")
  (ar-th-transpose 'slashedparen no-delimiters))

(defun ar-sort-slashedparen-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts slashedparens in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'slashedparen reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-slashedparen-atpt (&optional arg)
  "Return t if a SLASHEDPAREN at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-slashedparen-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-slashedparen-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-symbol-atpt (&optional no-delimiters)
  "Returns symbol at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'symbol no-delimiters))

(defun ar-bounds-of-symbol-atpt (&optional no-delimiters)
  "Returns a list, borders of symbol if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'symbol no-delimiters))

(defun ar-symbol-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position SYMBOL at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'symbol no-delimiters))

(defun ar-symbol-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of SYMBOL at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'symbol no-delimiters))

(defun ar-beginning-of-symbol-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class SYMBOL at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'symbol no-delimiters))

(defun ar-end-of-symbol-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'symbol no-delimiters))

(defun ar-in-symbol-p-atpt (&optional no-delimiters)
  "Returns bounds of SYMBOL at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'symbol no-delimiters))

(defun ar-length-of-symbol-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'symbol no-delimiters))

(defun ar-copy-symbol-atpt (&optional no-delimiters)
  "Returns a copy of SYMBOL at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'symbol no-delimiters))

(defun ar-delete-symbol-atpt (&optional no-delimiters)
  "Deletes SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-delete 'symbol no-delimiters))

(defun ar-delete-symbol-in-region (beg end)
  "Deletes SYMBOL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'symbol beg end))

(defun ar-blok-symbol-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around symbol.
  Returns blok or nil if no SYMBOL at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'symbol no-delimiters))

(defun ar-backslashparen-symbol-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around symbol at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'symbol no-delimiters))

(defun ar-doublebackslash-symbol-atpt (&optional no-delimiters)
  "Puts doubled backslashes around SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'symbol no-delimiters))

(defun ar-doubleslash-symbol-atpt (&optional no-delimiters)
  "Puts doubled slashes around SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'symbol no-delimiters))

(defun ar-doublebackslashparen-symbol-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'symbol no-delimiters))

(defun ar-doublebacktick-symbol-atpt (&optional no-delimiters)
  "Provides double backticks around SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'symbol no-delimiters))

(defun ar-slashparen-symbol-atpt (&optional no-delimiters)
  "Provides slashed parentheses around SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'symbol no-delimiters))

(defun ar-slashparen-symbol-atpt (&optional no-delimiters)
  "Provides slashed parentheses around SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'symbol no-delimiters))

(defun ar-comment-symbol-atpt (&optional no-delimiters)
  "Comments SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-comment 'symbol no-delimiters))

(defun ar-commatize-symbol-atpt (&optional no-delimiters)
  "Put a comma after SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-commatize 'symbol no-delimiters))

(defun ar-quote-symbol-atpt (&optional no-delimiters)
  "Put a singlequote before SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-quote 'symbol no-delimiters))


(defun ar-mark-symbol-atpt (&optional no-delimiters)
  "Marks SYMBOL at point if any. "
  (interactive "p")
  (ar-th-mark 'symbol))

(defun ar-hide-symbol-atpt (&optional no-delimiters)
  "Hides SYMBOL at point. "
  (interactive "p")
  (ar-th-hide 'symbol))

(defun ar-show-symbol-atpt (&optional no-delimiters)
  "Shows hidden SYMBOL at point. "
  (interactive "p")
  (ar-th-show 'symbol))

(defun ar-hide-show-symbol-atpt (&optional no-delimiters)
  "Alternatively hides or shows SYMBOL at point. "
  (interactive "p")
  (ar-th-hide-show 'symbol))

(defun ar-highlight-symbol-atpt-mode (&optional no-delimiters)
  "Toggles symbol-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'symbol no-delimiters))

(defun ar-kill-symbol-atpt (&optional no-delimiters)
  "Kills SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-kill 'symbol no-delimiters))

(defun ar-curvedsinglequote-symbol-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'symbol no-delimiters))

(defun ar-separate-symbol-atpt (&optional no-delimiters)
  "Separates SYMBOL at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'symbol no-delimiters))

(defun ar-triplequotedq-symbol-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around symbol. "
  (interactive "*P")
  (ar-th-triplequotedq 'symbol no-delimiters))

(defun ar-triplequotesq-symbol-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around symbol. "
  (interactive "*P")
  (ar-th-triplequotesq 'symbol no-delimiters))

(defun ar-triplebacktick-symbol-atpt (&optional no-delimiters)
  "Deletes symbol at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'symbol no-delimiters))

(defun ar-trim-symbol-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'symbol no-delimiters t t))

(defun ar-left-trim-symbol-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'symbol no-delimiters t))

(defun ar-right-trim-symbol-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'symbol n no-delimiters nil t))

(defun ar-underscore-symbol-atpt (&optional no-delimiters)
  "Put underscore char around SYMBOL. "
  (interactive "*P")
  (ar-th-underscore 'symbol no-delimiters))

(defun ar-forward-symbol-atpt (&optional no-delimiters)
  "Moves forward over SYMBOL at point if any, does nothing otherwise.
Returns end position of SYMBOL "
  (interactive "p")
  (ar-th-forward 'symbol no-delimiters))

(defun ar-backward-symbol-atpt (&optional no-delimiters)
  "Moves backward over SYMBOL before point if any, does nothing otherwise.
Returns beginning position of SYMBOL "
  (interactive "p")
  (ar-th-backward 'symbol no-delimiters))

(defun ar-transpose-symbol-atpt (&optional no-delimiters)
  "Transposes SYMBOL with SYMBOL before point if any. "
  (interactive "*P")
  (ar-th-transpose 'symbol no-delimiters))

(defun ar-sort-symbol-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts symbols in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'symbol reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-symbol-atpt (&optional arg)
  "Return t if a SYMBOL at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-symbol-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-symbol-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-tabledata-atpt (&optional no-delimiters)
  "Returns tabledata at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'tabledata no-delimiters))

(defun ar-bounds-of-tabledata-atpt (&optional no-delimiters)
  "Returns a list, borders of tabledata if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'tabledata no-delimiters))

(defun ar-tabledata-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position TABLEDATA at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'tabledata no-delimiters))

(defun ar-tabledata-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of TABLEDATA at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'tabledata no-delimiters))

(defun ar-beginning-of-tabledata-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class TABLEDATA at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'tabledata no-delimiters))

(defun ar-end-of-tabledata-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class TABLEDATA at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'tabledata no-delimiters))

(defun ar-in-tabledata-p-atpt (&optional no-delimiters)
  "Returns bounds of TABLEDATA at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'tabledata no-delimiters))

(defun ar-length-of-tabledata-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class TABLEDATA at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'tabledata no-delimiters))

(defun ar-copy-tabledata-atpt (&optional no-delimiters)
  "Returns a copy of TABLEDATA at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'tabledata no-delimiters))

(defun ar-delete-tabledata-atpt (&optional no-delimiters)
  "Deletes TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-delete 'tabledata no-delimiters))

(defun ar-delete-tabledata-in-region (beg end)
  "Deletes TABLEDATA at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'tabledata beg end))

(defun ar-blok-tabledata-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around tabledata.
  Returns blok or nil if no TABLEDATA at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'tabledata no-delimiters))

(defun ar-backslashparen-tabledata-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around tabledata at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'tabledata no-delimiters))

(defun ar-doublebackslash-tabledata-atpt (&optional no-delimiters)
  "Puts doubled backslashes around TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'tabledata no-delimiters))

(defun ar-doubleslash-tabledata-atpt (&optional no-delimiters)
  "Puts doubled slashes around TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'tabledata no-delimiters))

(defun ar-doublebackslashparen-tabledata-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'tabledata no-delimiters))

(defun ar-doublebacktick-tabledata-atpt (&optional no-delimiters)
  "Provides double backticks around TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'tabledata no-delimiters))

(defun ar-slashparen-tabledata-atpt (&optional no-delimiters)
  "Provides slashed parentheses around TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'tabledata no-delimiters))

(defun ar-slashparen-tabledata-atpt (&optional no-delimiters)
  "Provides slashed parentheses around TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'tabledata no-delimiters))

(defun ar-comment-tabledata-atpt (&optional no-delimiters)
  "Comments TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-comment 'tabledata no-delimiters))

(defun ar-commatize-tabledata-atpt (&optional no-delimiters)
  "Put a comma after TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-commatize 'tabledata no-delimiters))

(defun ar-quote-tabledata-atpt (&optional no-delimiters)
  "Put a singlequote before TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-quote 'tabledata no-delimiters))


(defun ar-mark-tabledata-atpt (&optional no-delimiters)
  "Marks TABLEDATA at point if any. "
  (interactive "p")
  (ar-th-mark 'tabledata))

(defun ar-hide-tabledata-atpt (&optional no-delimiters)
  "Hides TABLEDATA at point. "
  (interactive "p")
  (ar-th-hide 'tabledata))

(defun ar-show-tabledata-atpt (&optional no-delimiters)
  "Shows hidden TABLEDATA at point. "
  (interactive "p")
  (ar-th-show 'tabledata))

(defun ar-hide-show-tabledata-atpt (&optional no-delimiters)
  "Alternatively hides or shows TABLEDATA at point. "
  (interactive "p")
  (ar-th-hide-show 'tabledata))

(defun ar-highlight-tabledata-atpt-mode (&optional no-delimiters)
  "Toggles tabledata-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'tabledata no-delimiters))

(defun ar-kill-tabledata-atpt (&optional no-delimiters)
  "Kills TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-kill 'tabledata no-delimiters))

(defun ar-curvedsinglequote-tabledata-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'tabledata no-delimiters))

(defun ar-separate-tabledata-atpt (&optional no-delimiters)
  "Separates TABLEDATA at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'tabledata no-delimiters))

(defun ar-triplequotedq-tabledata-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around tabledata. "
  (interactive "*P")
  (ar-th-triplequotedq 'tabledata no-delimiters))

(defun ar-triplequotesq-tabledata-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around tabledata. "
  (interactive "*P")
  (ar-th-triplequotesq 'tabledata no-delimiters))

(defun ar-triplebacktick-tabledata-atpt (&optional no-delimiters)
  "Deletes tabledata at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'tabledata no-delimiters))

(defun ar-trim-tabledata-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'tabledata no-delimiters t t))

(defun ar-left-trim-tabledata-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'tabledata no-delimiters t))

(defun ar-right-trim-tabledata-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'tabledata n no-delimiters nil t))

(defun ar-underscore-tabledata-atpt (&optional no-delimiters)
  "Put underscore char around TABLEDATA. "
  (interactive "*P")
  (ar-th-underscore 'tabledata no-delimiters))

(defun ar-forward-tabledata-atpt (&optional no-delimiters)
  "Moves forward over TABLEDATA at point if any, does nothing otherwise.
Returns end position of TABLEDATA "
  (interactive "p")
  (ar-th-forward 'tabledata no-delimiters))

(defun ar-backward-tabledata-atpt (&optional no-delimiters)
  "Moves backward over TABLEDATA before point if any, does nothing otherwise.
Returns beginning position of TABLEDATA "
  (interactive "p")
  (ar-th-backward 'tabledata no-delimiters))

(defun ar-transpose-tabledata-atpt (&optional no-delimiters)
  "Transposes TABLEDATA with TABLEDATA before point if any. "
  (interactive "*P")
  (ar-th-transpose 'tabledata no-delimiters))

(defun ar-sort-tabledata-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts tabledatas in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'tabledata reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-tabledata-atpt (&optional arg)
  "Return t if a TABLEDATA at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-tabledata-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-tabledata-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-xslstylesheet-atpt (&optional no-delimiters)
  "Returns xslstylesheet at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'xslstylesheet no-delimiters))

(defun ar-bounds-of-xslstylesheet-atpt (&optional no-delimiters)
  "Returns a list, borders of xslstylesheet if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'xslstylesheet no-delimiters))

(defun ar-xslstylesheet-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position XSLSTYLESHEET at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'xslstylesheet no-delimiters))

(defun ar-xslstylesheet-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of XSLSTYLESHEET at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'xslstylesheet no-delimiters))

(defun ar-beginning-of-xslstylesheet-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class XSLSTYLESHEET at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'xslstylesheet no-delimiters))

(defun ar-end-of-xslstylesheet-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class XSLSTYLESHEET at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'xslstylesheet no-delimiters))

(defun ar-in-xslstylesheet-p-atpt (&optional no-delimiters)
  "Returns bounds of XSLSTYLESHEET at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'xslstylesheet no-delimiters))

(defun ar-length-of-xslstylesheet-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class XSLSTYLESHEET at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'xslstylesheet no-delimiters))

(defun ar-copy-xslstylesheet-atpt (&optional no-delimiters)
  "Returns a copy of XSLSTYLESHEET at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'xslstylesheet no-delimiters))

(defun ar-delete-xslstylesheet-atpt (&optional no-delimiters)
  "Deletes XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-delete 'xslstylesheet no-delimiters))

(defun ar-delete-xslstylesheet-in-region (beg end)
  "Deletes XSLSTYLESHEET at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'xslstylesheet beg end))

(defun ar-blok-xslstylesheet-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around xslstylesheet.
  Returns blok or nil if no XSLSTYLESHEET at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'xslstylesheet no-delimiters))

(defun ar-backslashparen-xslstylesheet-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around xslstylesheet at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'xslstylesheet no-delimiters))

(defun ar-doublebackslash-xslstylesheet-atpt (&optional no-delimiters)
  "Puts doubled backslashes around XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'xslstylesheet no-delimiters))

(defun ar-doubleslash-xslstylesheet-atpt (&optional no-delimiters)
  "Puts doubled slashes around XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'xslstylesheet no-delimiters))

(defun ar-doublebackslashparen-xslstylesheet-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'xslstylesheet no-delimiters))

(defun ar-doublebacktick-xslstylesheet-atpt (&optional no-delimiters)
  "Provides double backticks around XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'xslstylesheet no-delimiters))

(defun ar-slashparen-xslstylesheet-atpt (&optional no-delimiters)
  "Provides slashed parentheses around XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'xslstylesheet no-delimiters))

(defun ar-slashparen-xslstylesheet-atpt (&optional no-delimiters)
  "Provides slashed parentheses around XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'xslstylesheet no-delimiters))

(defun ar-comment-xslstylesheet-atpt (&optional no-delimiters)
  "Comments XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-comment 'xslstylesheet no-delimiters))

(defun ar-commatize-xslstylesheet-atpt (&optional no-delimiters)
  "Put a comma after XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-commatize 'xslstylesheet no-delimiters))

(defun ar-quote-xslstylesheet-atpt (&optional no-delimiters)
  "Put a singlequote before XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-quote 'xslstylesheet no-delimiters))


(defun ar-mark-xslstylesheet-atpt (&optional no-delimiters)
  "Marks XSLSTYLESHEET at point if any. "
  (interactive "p")
  (ar-th-mark 'xslstylesheet))

(defun ar-hide-xslstylesheet-atpt (&optional no-delimiters)
  "Hides XSLSTYLESHEET at point. "
  (interactive "p")
  (ar-th-hide 'xslstylesheet))

(defun ar-show-xslstylesheet-atpt (&optional no-delimiters)
  "Shows hidden XSLSTYLESHEET at point. "
  (interactive "p")
  (ar-th-show 'xslstylesheet))

(defun ar-hide-show-xslstylesheet-atpt (&optional no-delimiters)
  "Alternatively hides or shows XSLSTYLESHEET at point. "
  (interactive "p")
  (ar-th-hide-show 'xslstylesheet))

(defun ar-highlight-xslstylesheet-atpt-mode (&optional no-delimiters)
  "Toggles xslstylesheet-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'xslstylesheet no-delimiters))

(defun ar-kill-xslstylesheet-atpt (&optional no-delimiters)
  "Kills XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-kill 'xslstylesheet no-delimiters))

(defun ar-curvedsinglequote-xslstylesheet-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'xslstylesheet no-delimiters))

(defun ar-separate-xslstylesheet-atpt (&optional no-delimiters)
  "Separates XSLSTYLESHEET at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'xslstylesheet no-delimiters))

(defun ar-triplequotedq-xslstylesheet-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around xslstylesheet. "
  (interactive "*P")
  (ar-th-triplequotedq 'xslstylesheet no-delimiters))

(defun ar-triplequotesq-xslstylesheet-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around xslstylesheet. "
  (interactive "*P")
  (ar-th-triplequotesq 'xslstylesheet no-delimiters))

(defun ar-triplebacktick-xslstylesheet-atpt (&optional no-delimiters)
  "Deletes xslstylesheet at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'xslstylesheet no-delimiters))

(defun ar-trim-xslstylesheet-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'xslstylesheet no-delimiters t t))

(defun ar-left-trim-xslstylesheet-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'xslstylesheet no-delimiters t))

(defun ar-right-trim-xslstylesheet-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'xslstylesheet n no-delimiters nil t))

(defun ar-underscore-xslstylesheet-atpt (&optional no-delimiters)
  "Put underscore char around XSLSTYLESHEET. "
  (interactive "*P")
  (ar-th-underscore 'xslstylesheet no-delimiters))

(defun ar-forward-xslstylesheet-atpt (&optional no-delimiters)
  "Moves forward over XSLSTYLESHEET at point if any, does nothing otherwise.
Returns end position of XSLSTYLESHEET "
  (interactive "p")
  (ar-th-forward 'xslstylesheet no-delimiters))

(defun ar-backward-xslstylesheet-atpt (&optional no-delimiters)
  "Moves backward over XSLSTYLESHEET before point if any, does nothing otherwise.
Returns beginning position of XSLSTYLESHEET "
  (interactive "p")
  (ar-th-backward 'xslstylesheet no-delimiters))

(defun ar-transpose-xslstylesheet-atpt (&optional no-delimiters)
  "Transposes XSLSTYLESHEET with XSLSTYLESHEET before point if any. "
  (interactive "*P")
  (ar-th-transpose 'xslstylesheet no-delimiters))

(defun ar-sort-xslstylesheet-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts xslstylesheets in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'xslstylesheet reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-xslstylesheet-atpt (&optional arg)
  "Return t if a XSLSTYLESHEET at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-xslstylesheet-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-xslstylesheet-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

(defun ar-xsltemplate-atpt (&optional no-delimiters)
  "Returns xsltemplate at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns objects without delimiters"
  (interactive "p")
  (ar-th 'xsltemplate no-delimiters))

(defun ar-bounds-of-xsltemplate-atpt (&optional no-delimiters)
  "Returns a list, borders of xsltemplate if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns bounds without delimiters"
  (interactive "p")
  (ar-th-bounds 'xsltemplate no-delimiters))

(defun ar-xsltemplate-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position XSLTEMPLATE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-beg 'xsltemplate no-delimiters))

(defun ar-xsltemplate-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of XSLTEMPLATE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns end position at delimiter "
  (interactive "p")
  (ar-th-end 'xsltemplate no-delimiters))

(defun ar-beginning-of-xsltemplate-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class XSLTEMPLATE at point if any, nil otherwise.

Optional \\[universal-argument], from a programm '(4), returns start position after delimiter "
  (interactive "p")
  (ar-th-gotobeg 'xsltemplate no-delimiters))

(defun ar-end-of-xsltemplate-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class XSLTEMPLATE at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-gotoend 'xsltemplate no-delimiters))

(defun ar-in-xsltemplate-p-atpt (&optional no-delimiters)
  "Returns bounds of XSLTEMPLATE at point, a list, if inside, nil otherwise. "
  (interactive "p")
  (ar-th-bounds 'xsltemplate no-delimiters))

(defun ar-length-of-xsltemplate-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class XSLTEMPLATE at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-length 'xsltemplate no-delimiters))

(defun ar-copy-xsltemplate-atpt (&optional no-delimiters)
  "Returns a copy of XSLTEMPLATE at point if any, nil otherwise. "
  (interactive "p")
  (ar-th-copy 'xsltemplate no-delimiters))

(defun ar-delete-xsltemplate-atpt (&optional no-delimiters)
  "Deletes XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-delete 'xsltemplate no-delimiters))

(defun ar-delete-xsltemplate-in-region (beg end)
  "Deletes XSLTEMPLATE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'xsltemplate beg end))

(defun ar-blok-xsltemplate-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around xsltemplate.
  Returns blok or nil if no XSLTEMPLATE at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'xsltemplate no-delimiters))

(defun ar-backslashparen-xsltemplate-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around xsltemplate at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-backslashparen 'xsltemplate no-delimiters))

(defun ar-doublebackslash-xsltemplate-atpt (&optional no-delimiters)
  "Puts doubled backslashes around XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'xsltemplate no-delimiters))

(defun ar-doubleslash-xsltemplate-atpt (&optional no-delimiters)
  "Puts doubled slashes around XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'xsltemplate no-delimiters))

(defun ar-doublebackslashparen-xsltemplate-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'xsltemplate no-delimiters))

(defun ar-doublebacktick-xsltemplate-atpt (&optional no-delimiters)
  "Provides double backticks around XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'xsltemplate no-delimiters))

(defun ar-slashparen-xsltemplate-atpt (&optional no-delimiters)
  "Provides slashed parentheses around XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'xsltemplate no-delimiters))

(defun ar-slashparen-xsltemplate-atpt (&optional no-delimiters)
  "Provides slashed parentheses around XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'xsltemplate no-delimiters))

(defun ar-comment-xsltemplate-atpt (&optional no-delimiters)
  "Comments XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-comment 'xsltemplate no-delimiters))

(defun ar-commatize-xsltemplate-atpt (&optional no-delimiters)
  "Put a comma after XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-commatize 'xsltemplate no-delimiters))

(defun ar-quote-xsltemplate-atpt (&optional no-delimiters)
  "Put a singlequote before XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-quote 'xsltemplate no-delimiters))


(defun ar-mark-xsltemplate-atpt (&optional no-delimiters)
  "Marks XSLTEMPLATE at point if any. "
  (interactive "p")
  (ar-th-mark 'xsltemplate))

(defun ar-hide-xsltemplate-atpt (&optional no-delimiters)
  "Hides XSLTEMPLATE at point. "
  (interactive "p")
  (ar-th-hide 'xsltemplate))

(defun ar-show-xsltemplate-atpt (&optional no-delimiters)
  "Shows hidden XSLTEMPLATE at point. "
  (interactive "p")
  (ar-th-show 'xsltemplate))

(defun ar-hide-show-xsltemplate-atpt (&optional no-delimiters)
  "Alternatively hides or shows XSLTEMPLATE at point. "
  (interactive "p")
  (ar-th-hide-show 'xsltemplate))

(defun ar-highlight-xsltemplate-atpt-mode (&optional no-delimiters)
  "Toggles xsltemplate-highlight-atpt-mode "
  (interactive "p")
  (ar-th-highlight 'xsltemplate no-delimiters))

(defun ar-kill-xsltemplate-atpt (&optional no-delimiters)
  "Kills XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-kill 'xsltemplate no-delimiters))

(defun ar-curvedsinglequote-xsltemplate-atpt (&optional no-delimiters)
  "Singlequotes alnum at point if any. "
  (interactive "*P")
  (ar-th-curvedsinglequote 'xsltemplate no-delimiters))

(defun ar-separate-xsltemplate-atpt (&optional no-delimiters)
  "Separates XSLTEMPLATE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'xsltemplate no-delimiters))

(defun ar-triplequotedq-xsltemplate-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around xsltemplate. "
  (interactive "*P")
  (ar-th-triplequotedq 'xsltemplate no-delimiters))

(defun ar-triplequotesq-xsltemplate-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around xsltemplate. "
  (interactive "*P")
  (ar-th-triplequotesq 'xsltemplate no-delimiters))

(defun ar-triplebacktick-xsltemplate-atpt (&optional no-delimiters)
  "Deletes xsltemplate at point if any.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-triplebacktick 'xsltemplate no-delimiters))

(defun ar-trim-xsltemplate-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'xsltemplate no-delimiters t t))

(defun ar-left-trim-xsltemplate-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'xsltemplate no-delimiters t))

(defun ar-right-trim-xsltemplate-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'xsltemplate n no-delimiters nil t))

(defun ar-underscore-xsltemplate-atpt (&optional no-delimiters)
  "Put underscore char around XSLTEMPLATE. "
  (interactive "*P")
  (ar-th-underscore 'xsltemplate no-delimiters))

(defun ar-forward-xsltemplate-atpt (&optional no-delimiters)
  "Moves forward over XSLTEMPLATE at point if any, does nothing otherwise.
Returns end position of XSLTEMPLATE "
  (interactive "p")
  (ar-th-forward 'xsltemplate no-delimiters))

(defun ar-backward-xsltemplate-atpt (&optional no-delimiters)
  "Moves backward over XSLTEMPLATE before point if any, does nothing otherwise.
Returns beginning position of XSLTEMPLATE "
  (interactive "p")
  (ar-th-backward 'xsltemplate no-delimiters))

(defun ar-transpose-xsltemplate-atpt (&optional no-delimiters)
  "Transposes XSLTEMPLATE with XSLTEMPLATE before point if any. "
  (interactive "*P")
  (ar-th-transpose 'xsltemplate no-delimiters))

(defun ar-sort-xsltemplate-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts xsltemplates in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from ‘sort-subr’, for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'xsltemplate reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-xsltemplate-atpt (&optional arg)
  "Return t if a XSLTEMPLATE at point exists, nil otherwise "
  (interactive "p")
  (let* ((beg (funcall (intern-soft (concat "ar-xsltemplate-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-xsltemplate-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
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
  (interactive "p")
  (skip-chars-backward "=\" \t\r\n\f")
  (forward-char -1))

(defun ar-go-in-statement-backward-to-beg-of-symbol ()
  (ar-go-in-statement-backward)
  (ar-th-gotobeg 'symbol))

(defun ar-value-of-mlattribut-atpt (&optional arg)
  (interactive "p")
  "Return the value of attribute under cursor if any."
  (ar-end-of-mlattribut-atpt)
  (forward-char -1)
  (let ((value (ar-string-atpt arg)))
    (when arg (message "%s" value))
    value))

(defun ar-name-of-mlattribut-atpt (&optional arg) 
  (interactive "p")
  (ar-beginning-of-mlattribut-atpt)
  (let ((name (ar-alnum-atpt)))
    (when arg (message "%s" name))
    name))

(defun ar-mlattribut-or-name-atpt (&optional arg)
  "Returns mlattribut-atpt at point if any, nil otherwise.
  With \C-u ar-name-of-mlattribut-atpt is called "
  (interactive "P")
  (if (eq 4 (prefix-numeric-value arg))
    (ar-name-of-mlattribut-atpt)
  (ar-th 'mlattribut arg)))




(provide 'thing-at-point-markup)
;;; thing-at-point-markup.el ends here
