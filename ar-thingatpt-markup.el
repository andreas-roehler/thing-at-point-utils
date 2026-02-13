;;; ar-thingatpt-markup.el --- th-at-point edit functions -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2026 Andreas Röhler, unless
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

(require 'ar-thingatpt-utils-core)
(require 'ar-thingatpt-basic-definitions)
;; ar-thingatpt-utils-markup ar-atpt-markup-list start

;;;###autoload
(defun ar-un-beginendquote-atpt (&optional arg)
  "Remove markups provided by beginendquote. "
  (interactive "P*")
  (ar-th-un-ml 'beginendquote arg arg))

;;;###autoload
(defun ar-un-blok-atpt (&optional arg)
  "Remove markups provided by blok. "
  (interactive "P*")
  (ar-th-un-ml 'blok arg arg))

;;;###autoload
(defun ar-un-doublebackslashed-atpt (&optional arg)
  "Remove markups provided by doublebackslashed. "
  (interactive "P*")
  (ar-th-un-ml 'doublebackslashed arg arg))

;;;###autoload
(defun ar-un-doublebackslashedparen-atpt (&optional arg)
  "Remove markups provided by doublebackslashedparen. "
  (interactive "P*")
  (ar-th-un-ml 'doublebackslashedparen arg arg))

;;;###autoload
(defun ar-un-doubleslashed-atpt (&optional arg)
  "Remove markups provided by doubleslashed. "
  (interactive "P*")
  (ar-th-un-ml 'doubleslashed arg arg))

;;;###autoload
(defun ar-un-tabledata-atpt (&optional arg)
  "Remove markups provided by tabledata. "
  (interactive "P*")
  (ar-th-un-ml 'tabledata arg arg))

;; ar-thingatpt-utils-markup: ar-atpt-markup-list end


;; ar-thingatpt-utils-delimiters-core ar-atpt-markup-list start

;;;###autoload
(defun ar-beginendquote-atpt (&optional no-delimiters)
  "Returns beginendquote at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'beginendquote no-delimiters))

;;;###autoload
(defun ar-bounds-of-beginendquote-atpt (&optional no-delimiters)
  "Returns a list, borders of beginendquote if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'beginendquote no-delimiters))

;;;###autoload
(defun ar-beginendquote-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BEGINENDQUOTE.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'beginendquote no-delimiters))

;;;###autoload
(defun ar-beginendquote-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BEGINENDQUOTE.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'beginendquote no-delimiters))

;;;###autoload
(defun ar-beginning-of-beginendquote-atpt (&optional no-delimiters)
  "Goto beginning of BEGINENDQUOTE.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'beginendquote no-delimiters))

;;;###autoload
(defun ar-end-of-beginendquote-atpt (&optional no-delimiters)
  "Goto end of BEGINENDQUOTE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'beginendquote no-delimiters))

;;;###autoload
(defun ar-in-beginendquote-p-atpt (&optional no-delimiters)
  "Returns bounds of BEGINENDQUOTE if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'beginendquote no-delimiters))

;;;###autoload
(defun ar-length-of-beginendquote-atpt (&optional no-delimiters)
  "Returns beginning of BEGINENDQUOTE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'beginendquote no-delimiters))

;;;###autoload
(defun ar-copy-beginendquote-atpt (&optional no-delimiters)
  "Returns a copy of BEGINENDQUOTE. "
  (interactive "P")
  (ar-th-copy 'beginendquote no-delimiters))

;;;###autoload
(defun ar-delete-beginendquote-atpt (&optional no-delimiters)
  "Deletes BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-delete 'beginendquote no-delimiters))

;;;###autoload
(defun ar-delete-beginendquote-in-region (beg end)
  "Deletes BEGINENDQUOTE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'beginendquote beg end))

;;;###autoload
(defun ar-blok-beginendquote-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around beginendquote.
  Returns blok or nil if no BEGINENDQUOTE at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'beginendquote no-delimiters))

;;;###autoload
(defun ar-backslashparen-beginendquote-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around beginendquote at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'beginendquote no-delimiters))

;;;###autoload
(defun ar-doublebackslash-beginendquote-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'beginendquote no-delimiters))

;;;###autoload
(defun ar-doubleslash-beginendquote-atpt (&optional no-delimiters)
  "Puts doubled slashes around BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'beginendquote no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-beginendquote-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'beginendquote no-delimiters))

;;;###autoload
(defun ar-doublebacktick-beginendquote-atpt (&optional no-delimiters)
  "Provides double backticks around BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'beginendquote no-delimiters))

;;;###autoload
(defun ar-slashparen-beginendquote-atpt (&optional no-delimiters)
  "Provides slashed parentheses around BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'beginendquote no-delimiters))

;;;###autoload
(defun ar-comment-beginendquote-atpt (&optional no-delimiters)
  "Comments BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-comment 'beginendquote no-delimiters))

;;;###autoload
(defun ar-commatize-beginendquote-atpt (&optional no-delimiters)
  "Put a comma after BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-commatize 'beginendquote no-delimiters))

;;;###autoload
(defun ar-quote-beginendquote-atpt (&optional no-delimiters)
  "Put a singlequote before BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-quote 'beginendquote no-delimiters))

;;;###autoload
(defun ar-mark-beginendquote-atpt (&optional no-delimiters)
  "Marks BEGINENDQUOTE at point if any. "
  (interactive "P")
  (ar-th-mark 'beginendquote no-delimiters))

;;;###autoload
(defun ar-hide-beginendquote-atpt (&optional no-delimiters)
  "Hides BEGINENDQUOTE at point. "
  (interactive "P")
  (ar-th-hide 'beginendquote nil nil no-delimiters))

;;;###autoload
(defun ar-show-beginendquote-atpt (&optional no-delimiters)
  "Shows hidden BEGINENDQUOTE at point. "
  (interactive "P")
  (ar-th-show 'beginendquote nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-beginendquote-atpt (&optional no-delimiters)
  "Alternatively hides or shows BEGINENDQUOTE at point. "
  (interactive "P")
  (ar-th-hide-show 'beginendquote nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-beginendquote-atpt-mode (&optional no-delimiters)
  "Toggles beginendquote-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'beginendquote no-delimiters))

;;;###autoload
(defun ar-kill-beginendquote-atpt (&optional no-delimiters)
  "Kills BEGINENDQUOTE at point if any. "
  (interactive "*P")
  (ar-th-kill 'beginendquote no-delimiters))

;;;###autoload
(defun ar-separate-beginendquote-atpt (&optional no-delimiters)
  "Separates BEGINENDQUOTE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'beginendquote no-delimiters))

;;;###autoload
(defun ar-triplequotedq-beginendquote-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around beginendquote. "
  (interactive "*P")
  (ar-th-triplequotedq 'beginendquote no-delimiters))

;;;###autoload
(defun ar-triplequotesq-beginendquote-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around beginendquote. "
  (interactive "*P")
  (ar-th-triplequotesq 'beginendquote no-delimiters))

;;;###autoload
(defun ar-triplebacktick-beginendquote-atpt (&optional no-delimiters)
  "Triplebacktick beginendquote at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'beginendquote no-delimiters))

;;;###autoload
(defun ar-trim-beginendquote-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'beginendquote no-delimiters t t))

;;;###autoload
(defun ar-left-trim-beginendquote-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'beginendquote no-delimiters t))

;;;###autoload
(defun ar-right-trim-beginendquote-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'beginendquote no-delimiters nil t))

;;;###autoload
(defun ar-underscore-beginendquote-atpt (&optional no-delimiters)
  "Put underscore char around BEGINENDQUOTE. "
  (interactive "*P")
  (ar-th-underscore 'beginendquote no-delimiters))

;;;###autoload
(defun ar-forward-beginendquote-atpt (&optional no-delimiters)
  "Moves forward over BEGINENDQUOTE at point if any, does nothing otherwise.
Returns end position of BEGINENDQUOTE "
  (interactive "P")
  (ar-th-forward 'beginendquote no-delimiters))

;;;###autoload
(defun ar-backward-beginendquote-atpt (&optional no-delimiters)
  "Moves backward over BEGINENDQUOTE.
Returns beginning position of BEGINENDQUOTE "
  (interactive "P")
  (ar-th-backward 'beginendquote no-delimiters))

;;;###autoload
(defun ar-transpose-beginendquote-atpt (&optional no-delimiters)
  "Transposes BEGINENDQUOTE with BEGINENDQUOTE before point if any. "
  (interactive "*P")
  (ar-th-transpose 'beginendquote no-delimiters))

;;;###autoload
(defun ar-sort-beginendquote-atpt (&optional reverse)
  "Sorts beginendquotes in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'beginendquote reverse))

;;;###autoload
(defun ar-check-beginendquote-atpt (&optional arg)
  "Return t if a BEGINENDQUOTE at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-beginendquote-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-beginendquote-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-blok-atpt (&optional no-delimiters)
  "Returns blok at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'blok no-delimiters))

;;;###autoload
(defun ar-bounds-of-blok-atpt (&optional no-delimiters)
  "Returns a list, borders of blok if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'blok no-delimiters))

;;;###autoload
(defun ar-blok-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BLOK.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'blok no-delimiters))

;;;###autoload
(defun ar-blok-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BLOK.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'blok no-delimiters))

;;;###autoload
(defun ar-beginning-of-blok-atpt (&optional no-delimiters)
  "Goto beginning of BLOK.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'blok no-delimiters))

;;;###autoload
(defun ar-end-of-blok-atpt (&optional no-delimiters)
  "Goto end of BLOK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'blok no-delimiters))

;;;###autoload
(defun ar-in-blok-p-atpt (&optional no-delimiters)
  "Returns bounds of BLOK if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'blok no-delimiters))

;;;###autoload
(defun ar-length-of-blok-atpt (&optional no-delimiters)
  "Returns beginning of BLOK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'blok no-delimiters))

;;;###autoload
(defun ar-copy-blok-atpt (&optional no-delimiters)
  "Returns a copy of BLOK. "
  (interactive "P")
  (ar-th-copy 'blok no-delimiters))

;;;###autoload
(defun ar-delete-blok-atpt (&optional no-delimiters)
  "Deletes BLOK at point if any. "
  (interactive "*P")
  (ar-th-delete 'blok no-delimiters))

;;;###autoload
(defun ar-delete-blok-in-region (beg end)
  "Deletes BLOK at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'blok beg end))

;;;###autoload
(defun ar-blok-blok-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around blok.
  Returns blok or nil if no BLOK at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'blok no-delimiters))

;;;###autoload
(defun ar-backslashparen-blok-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around blok at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'blok no-delimiters))

;;;###autoload
(defun ar-doublebackslash-blok-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BLOK at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'blok no-delimiters))

;;;###autoload
(defun ar-doubleslash-blok-atpt (&optional no-delimiters)
  "Puts doubled slashes around BLOK at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'blok no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-blok-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BLOK at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'blok no-delimiters))

;;;###autoload
(defun ar-doublebacktick-blok-atpt (&optional no-delimiters)
  "Provides double backticks around BLOK at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'blok no-delimiters))

;;;###autoload
(defun ar-slashparen-blok-atpt (&optional no-delimiters)
  "Provides slashed parentheses around BLOK at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'blok no-delimiters))

;;;###autoload
(defun ar-comment-blok-atpt (&optional no-delimiters)
  "Comments BLOK at point if any. "
  (interactive "*P")
  (ar-th-comment 'blok no-delimiters))

;;;###autoload
(defun ar-commatize-blok-atpt (&optional no-delimiters)
  "Put a comma after BLOK at point if any. "
  (interactive "*P")
  (ar-th-commatize 'blok no-delimiters))

;;;###autoload
(defun ar-quote-blok-atpt (&optional no-delimiters)
  "Put a singlequote before BLOK at point if any. "
  (interactive "*P")
  (ar-th-quote 'blok no-delimiters))

;;;###autoload
(defun ar-mark-blok-atpt (&optional no-delimiters)
  "Marks BLOK at point if any. "
  (interactive "P")
  (ar-th-mark 'blok no-delimiters))

;;;###autoload
(defun ar-hide-blok-atpt (&optional no-delimiters)
  "Hides BLOK at point. "
  (interactive "P")
  (ar-th-hide 'blok nil nil no-delimiters))

;;;###autoload
(defun ar-show-blok-atpt (&optional no-delimiters)
  "Shows hidden BLOK at point. "
  (interactive "P")
  (ar-th-show 'blok nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-blok-atpt (&optional no-delimiters)
  "Alternatively hides or shows BLOK at point. "
  (interactive "P")
  (ar-th-hide-show 'blok nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-blok-atpt-mode (&optional no-delimiters)
  "Toggles blok-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'blok no-delimiters))

;;;###autoload
(defun ar-kill-blok-atpt (&optional no-delimiters)
  "Kills BLOK at point if any. "
  (interactive "*P")
  (ar-th-kill 'blok no-delimiters))

;;;###autoload
(defun ar-separate-blok-atpt (&optional no-delimiters)
  "Separates BLOK at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'blok no-delimiters))

;;;###autoload
(defun ar-triplequotedq-blok-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around blok. "
  (interactive "*P")
  (ar-th-triplequotedq 'blok no-delimiters))

;;;###autoload
(defun ar-triplequotesq-blok-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around blok. "
  (interactive "*P")
  (ar-th-triplequotesq 'blok no-delimiters))

;;;###autoload
(defun ar-triplebacktick-blok-atpt (&optional no-delimiters)
  "Triplebacktick blok at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'blok no-delimiters))

;;;###autoload
(defun ar-trim-blok-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'blok no-delimiters t t))

;;;###autoload
(defun ar-left-trim-blok-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'blok no-delimiters t))

;;;###autoload
(defun ar-right-trim-blok-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'blok no-delimiters nil t))

;;;###autoload
(defun ar-underscore-blok-atpt (&optional no-delimiters)
  "Put underscore char around BLOK. "
  (interactive "*P")
  (ar-th-underscore 'blok no-delimiters))

;;;###autoload
(defun ar-forward-blok-atpt (&optional no-delimiters)
  "Moves forward over BLOK at point if any, does nothing otherwise.
Returns end position of BLOK "
  (interactive "P")
  (ar-th-forward 'blok no-delimiters))

;;;###autoload
(defun ar-backward-blok-atpt (&optional no-delimiters)
  "Moves backward over BLOK.
Returns beginning position of BLOK "
  (interactive "P")
  (ar-th-backward 'blok no-delimiters))

;;;###autoload
(defun ar-transpose-blok-atpt (&optional no-delimiters)
  "Transposes BLOK with BLOK before point if any. "
  (interactive "*P")
  (ar-th-transpose 'blok no-delimiters))

;;;###autoload
(defun ar-sort-blok-atpt (&optional reverse)
  "Sorts bloks in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'blok reverse))

;;;###autoload
(defun ar-check-blok-atpt (&optional arg)
  "Return t if a BLOK at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-blok-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-blok-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-doublebackslashed-atpt (&optional no-delimiters)
  "Returns doublebackslashed at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-bounds-of-doublebackslashed-atpt (&optional no-delimiters)
  "Returns a list, borders of doublebackslashed if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-doublebackslashed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLEBACKSLASHED.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-doublebackslashed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLEBACKSLASHED.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-beginning-of-doublebackslashed-atpt (&optional no-delimiters)
  "Goto beginning of DOUBLEBACKSLASHED.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-end-of-doublebackslashed-atpt (&optional no-delimiters)
  "Goto end of DOUBLEBACKSLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-in-doublebackslashed-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLEBACKSLASHED if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-length-of-doublebackslashed-atpt (&optional no-delimiters)
  "Returns beginning of DOUBLEBACKSLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-copy-doublebackslashed-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLEBACKSLASHED. "
  (interactive "P")
  (ar-th-copy 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-delete-doublebackslashed-atpt (&optional no-delimiters)
  "Deletes DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-delete 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-delete-doublebackslashed-in-region (beg end)
  "Deletes DOUBLEBACKSLASHED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doublebackslashed beg end))

;;;###autoload
(defun ar-blok-doublebackslashed-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around doublebackslashed.
  Returns blok or nil if no DOUBLEBACKSLASHED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-backslashparen-doublebackslashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doublebackslashed at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-doublebackslash-doublebackslashed-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-doubleslash-doublebackslashed-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-doublebackslashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-doublebacktick-doublebackslashed-atpt (&optional no-delimiters)
  "Provides double backticks around DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-slashparen-doublebackslashed-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-comment-doublebackslashed-atpt (&optional no-delimiters)
  "Comments DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-comment 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-commatize-doublebackslashed-atpt (&optional no-delimiters)
  "Put a comma after DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-quote-doublebackslashed-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-quote 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-mark-doublebackslashed-atpt (&optional no-delimiters)
  "Marks DOUBLEBACKSLASHED at point if any. "
  (interactive "P")
  (ar-th-mark 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-hide-doublebackslashed-atpt (&optional no-delimiters)
  "Hides DOUBLEBACKSLASHED at point. "
  (interactive "P")
  (ar-th-hide 'doublebackslashed nil nil no-delimiters))

;;;###autoload
(defun ar-show-doublebackslashed-atpt (&optional no-delimiters)
  "Shows hidden DOUBLEBACKSLASHED at point. "
  (interactive "P")
  (ar-th-show 'doublebackslashed nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-doublebackslashed-atpt (&optional no-delimiters)
  "Alternatively hides or shows DOUBLEBACKSLASHED at point. "
  (interactive "P")
  (ar-th-hide-show 'doublebackslashed nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-doublebackslashed-atpt-mode (&optional no-delimiters)
  "Toggles doublebackslashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-kill-doublebackslashed-atpt (&optional no-delimiters)
  "Kills DOUBLEBACKSLASHED at point if any. "
  (interactive "*P")
  (ar-th-kill 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-separate-doublebackslashed-atpt (&optional no-delimiters)
  "Separates DOUBLEBACKSLASHED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-triplequotedq-doublebackslashed-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doublebackslashed. "
  (interactive "*P")
  (ar-th-triplequotedq 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-triplequotesq-doublebackslashed-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doublebackslashed. "
  (interactive "*P")
  (ar-th-triplequotesq 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-triplebacktick-doublebackslashed-atpt (&optional no-delimiters)
  "Triplebacktick doublebackslashed at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-trim-doublebackslashed-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashed no-delimiters t t))

;;;###autoload
(defun ar-left-trim-doublebackslashed-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashed no-delimiters t))

;;;###autoload
(defun ar-right-trim-doublebackslashed-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashed no-delimiters nil t))

;;;###autoload
(defun ar-underscore-doublebackslashed-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLEBACKSLASHED. "
  (interactive "*P")
  (ar-th-underscore 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-forward-doublebackslashed-atpt (&optional no-delimiters)
  "Moves forward over DOUBLEBACKSLASHED at point if any, does nothing otherwise.
Returns end position of DOUBLEBACKSLASHED "
  (interactive "P")
  (ar-th-forward 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-backward-doublebackslashed-atpt (&optional no-delimiters)
  "Moves backward over DOUBLEBACKSLASHED.
Returns beginning position of DOUBLEBACKSLASHED "
  (interactive "P")
  (ar-th-backward 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-transpose-doublebackslashed-atpt (&optional no-delimiters)
  "Transposes DOUBLEBACKSLASHED with DOUBLEBACKSLASHED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'doublebackslashed no-delimiters))

;;;###autoload
(defun ar-sort-doublebackslashed-atpt (&optional reverse)
  "Sorts doublebackslasheds in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'doublebackslashed reverse))

;;;###autoload
(defun ar-check-doublebackslashed-atpt (&optional arg)
  "Return t if a DOUBLEBACKSLASHED at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-doublebackslashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doublebackslashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-doublebackticked-atpt (&optional no-delimiters)
  "Returns doublebackticked at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-bounds-of-doublebackticked-atpt (&optional no-delimiters)
  "Returns a list, borders of doublebackticked if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-doublebackticked-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLEBACKTICKED.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-doublebackticked-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLEBACKTICKED.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-beginning-of-doublebackticked-atpt (&optional no-delimiters)
  "Goto beginning of DOUBLEBACKTICKED.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-end-of-doublebackticked-atpt (&optional no-delimiters)
  "Goto end of DOUBLEBACKTICKED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-in-doublebackticked-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLEBACKTICKED if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-length-of-doublebackticked-atpt (&optional no-delimiters)
  "Returns beginning of DOUBLEBACKTICKED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-copy-doublebackticked-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLEBACKTICKED. "
  (interactive "P")
  (ar-th-copy 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-delete-doublebackticked-atpt (&optional no-delimiters)
  "Deletes DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-delete 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-delete-doublebackticked-in-region (beg end)
  "Deletes DOUBLEBACKTICKED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doublebackticked beg end))

;;;###autoload
(defun ar-blok-doublebackticked-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around doublebackticked.
  Returns blok or nil if no DOUBLEBACKTICKED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-backslashparen-doublebackticked-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doublebackticked at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-doublebackslash-doublebackticked-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-doubleslash-doublebackticked-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-doublebackticked-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-doublebacktick-doublebackticked-atpt (&optional no-delimiters)
  "Provides double backticks around DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-slashparen-doublebackticked-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-comment-doublebackticked-atpt (&optional no-delimiters)
  "Comments DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-comment 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-commatize-doublebackticked-atpt (&optional no-delimiters)
  "Put a comma after DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-quote-doublebackticked-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-quote 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-mark-doublebackticked-atpt (&optional no-delimiters)
  "Marks DOUBLEBACKTICKED at point if any. "
  (interactive "P")
  (ar-th-mark 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-hide-doublebackticked-atpt (&optional no-delimiters)
  "Hides DOUBLEBACKTICKED at point. "
  (interactive "P")
  (ar-th-hide 'doublebackticked nil nil no-delimiters))

;;;###autoload
(defun ar-show-doublebackticked-atpt (&optional no-delimiters)
  "Shows hidden DOUBLEBACKTICKED at point. "
  (interactive "P")
  (ar-th-show 'doublebackticked nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-doublebackticked-atpt (&optional no-delimiters)
  "Alternatively hides or shows DOUBLEBACKTICKED at point. "
  (interactive "P")
  (ar-th-hide-show 'doublebackticked nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-doublebackticked-atpt-mode (&optional no-delimiters)
  "Toggles doublebackticked-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-kill-doublebackticked-atpt (&optional no-delimiters)
  "Kills DOUBLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-kill 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-separate-doublebackticked-atpt (&optional no-delimiters)
  "Separates DOUBLEBACKTICKED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-triplequotedq-doublebackticked-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doublebackticked. "
  (interactive "*P")
  (ar-th-triplequotedq 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-triplequotesq-doublebackticked-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doublebackticked. "
  (interactive "*P")
  (ar-th-triplequotesq 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-triplebacktick-doublebackticked-atpt (&optional no-delimiters)
  "Triplebacktick doublebackticked at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-trim-doublebackticked-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackticked no-delimiters t t))

;;;###autoload
(defun ar-left-trim-doublebackticked-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doublebackticked no-delimiters t))

;;;###autoload
(defun ar-right-trim-doublebackticked-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackticked no-delimiters nil t))

;;;###autoload
(defun ar-underscore-doublebackticked-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLEBACKTICKED. "
  (interactive "*P")
  (ar-th-underscore 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-forward-doublebackticked-atpt (&optional no-delimiters)
  "Moves forward over DOUBLEBACKTICKED at point if any, does nothing otherwise.
Returns end position of DOUBLEBACKTICKED "
  (interactive "P")
  (ar-th-forward 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-backward-doublebackticked-atpt (&optional no-delimiters)
  "Moves backward over DOUBLEBACKTICKED.
Returns beginning position of DOUBLEBACKTICKED "
  (interactive "P")
  (ar-th-backward 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-transpose-doublebackticked-atpt (&optional no-delimiters)
  "Transposes DOUBLEBACKTICKED with DOUBLEBACKTICKED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'doublebackticked no-delimiters))

;;;###autoload
(defun ar-sort-doublebackticked-atpt (&optional reverse)
  "Sorts doublebacktickeds in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'doublebackticked reverse))

;;;###autoload
(defun ar-check-doublebackticked-atpt (&optional arg)
  "Return t if a DOUBLEBACKTICKED at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-doublebackticked-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doublebackticked-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-doublebackslashedparen-atpt (&optional no-delimiters)
  "Returns doublebackslashedparen at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-bounds-of-doublebackslashedparen-atpt (&optional no-delimiters)
  "Returns a list, borders of doublebackslashedparen if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-doublebackslashedparen-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLEBACKSLASHEDPAREN.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-doublebackslashedparen-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLEBACKSLASHEDPAREN.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-beginning-of-doublebackslashedparen-atpt (&optional no-delimiters)
  "Goto beginning of DOUBLEBACKSLASHEDPAREN.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-end-of-doublebackslashedparen-atpt (&optional no-delimiters)
  "Goto end of DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-in-doublebackslashedparen-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLEBACKSLASHEDPAREN if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-length-of-doublebackslashedparen-atpt (&optional no-delimiters)
  "Returns beginning of DOUBLEBACKSLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-copy-doublebackslashedparen-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLEBACKSLASHEDPAREN. "
  (interactive "P")
  (ar-th-copy 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-delete-doublebackslashedparen-atpt (&optional no-delimiters)
  "Deletes DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-delete 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-delete-doublebackslashedparen-in-region (beg end)
  "Deletes DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doublebackslashedparen beg end))

;;;###autoload
(defun ar-blok-doublebackslashedparen-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around doublebackslashedparen.
  Returns blok or nil if no DOUBLEBACKSLASHEDPAREN at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-backslashparen-doublebackslashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doublebackslashedparen at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-doublebackslash-doublebackslashedparen-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-doubleslash-doublebackslashedparen-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-doublebackslashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-doublebacktick-doublebackslashedparen-atpt (&optional no-delimiters)
  "Provides double backticks around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-slashparen-doublebackslashedparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-comment-doublebackslashedparen-atpt (&optional no-delimiters)
  "Comments DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-comment 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-commatize-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put a comma after DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-commatize 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-quote-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-quote 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-mark-doublebackslashedparen-atpt (&optional no-delimiters)
  "Marks DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "P")
  (ar-th-mark 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-hide-doublebackslashedparen-atpt (&optional no-delimiters)
  "Hides DOUBLEBACKSLASHEDPAREN at point. "
  (interactive "P")
  (ar-th-hide 'doublebackslashedparen nil nil no-delimiters))

;;;###autoload
(defun ar-show-doublebackslashedparen-atpt (&optional no-delimiters)
  "Shows hidden DOUBLEBACKSLASHEDPAREN at point. "
  (interactive "P")
  (ar-th-show 'doublebackslashedparen nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-doublebackslashedparen-atpt (&optional no-delimiters)
  "Alternatively hides or shows DOUBLEBACKSLASHEDPAREN at point. "
  (interactive "P")
  (ar-th-hide-show 'doublebackslashedparen nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-doublebackslashedparen-atpt-mode (&optional no-delimiters)
  "Toggles doublebackslashedparen-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-kill-doublebackslashedparen-atpt (&optional no-delimiters)
  "Kills DOUBLEBACKSLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-kill 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-separate-doublebackslashedparen-atpt (&optional no-delimiters)
  "Separates DOUBLEBACKSLASHEDPAREN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-triplequotedq-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doublebackslashedparen. "
  (interactive "*P")
  (ar-th-triplequotedq 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-triplequotesq-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doublebackslashedparen. "
  (interactive "*P")
  (ar-th-triplequotesq 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-triplebacktick-doublebackslashedparen-atpt (&optional no-delimiters)
  "Triplebacktick doublebackslashedparen at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-trim-doublebackslashedparen-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashedparen no-delimiters t t))

;;;###autoload
(defun ar-left-trim-doublebackslashedparen-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashedparen no-delimiters t))

;;;###autoload
(defun ar-right-trim-doublebackslashedparen-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doublebackslashedparen no-delimiters nil t))

;;;###autoload
(defun ar-underscore-doublebackslashedparen-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLEBACKSLASHEDPAREN. "
  (interactive "*P")
  (ar-th-underscore 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-forward-doublebackslashedparen-atpt (&optional no-delimiters)
  "Moves forward over DOUBLEBACKSLASHEDPAREN at point if any, does nothing otherwise.
Returns end position of DOUBLEBACKSLASHEDPAREN "
  (interactive "P")
  (ar-th-forward 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-backward-doublebackslashedparen-atpt (&optional no-delimiters)
  "Moves backward over DOUBLEBACKSLASHEDPAREN.
Returns beginning position of DOUBLEBACKSLASHEDPAREN "
  (interactive "P")
  (ar-th-backward 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-transpose-doublebackslashedparen-atpt (&optional no-delimiters)
  "Transposes DOUBLEBACKSLASHEDPAREN with DOUBLEBACKSLASHEDPAREN before point if any. "
  (interactive "*P")
  (ar-th-transpose 'doublebackslashedparen no-delimiters))

;;;###autoload
(defun ar-sort-doublebackslashedparen-atpt (&optional reverse)
  "Sorts doublebackslashedparens in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'doublebackslashedparen reverse))

;;;###autoload
(defun ar-check-doublebackslashedparen-atpt (&optional arg)
  "Return t if a DOUBLEBACKSLASHEDPAREN at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-doublebackslashedparen-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doublebackslashedparen-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-doubleslashed-atpt (&optional no-delimiters)
  "Returns doubleslashed at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-bounds-of-doubleslashed-atpt (&optional no-delimiters)
  "Returns a list, borders of doubleslashed if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-doubleslashed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLESLASHED.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-doubleslashed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLESLASHED.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-beginning-of-doubleslashed-atpt (&optional no-delimiters)
  "Goto beginning of DOUBLESLASHED.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-end-of-doubleslashed-atpt (&optional no-delimiters)
  "Goto end of DOUBLESLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-in-doubleslashed-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLESLASHED if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-length-of-doubleslashed-atpt (&optional no-delimiters)
  "Returns beginning of DOUBLESLASHED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-copy-doubleslashed-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLESLASHED. "
  (interactive "P")
  (ar-th-copy 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-delete-doubleslashed-atpt (&optional no-delimiters)
  "Deletes DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-delete 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-delete-doubleslashed-in-region (beg end)
  "Deletes DOUBLESLASHED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doubleslashed beg end))

;;;###autoload
(defun ar-blok-doubleslashed-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around doubleslashed.
  Returns blok or nil if no DOUBLESLASHED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-backslashparen-doubleslashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doubleslashed at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-doublebackslash-doubleslashed-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-doubleslash-doubleslashed-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-doubleslashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-doublebacktick-doubleslashed-atpt (&optional no-delimiters)
  "Provides double backticks around DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-slashparen-doubleslashed-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-comment-doubleslashed-atpt (&optional no-delimiters)
  "Comments DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-comment 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-commatize-doubleslashed-atpt (&optional no-delimiters)
  "Put a comma after DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-quote-doubleslashed-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-quote 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-mark-doubleslashed-atpt (&optional no-delimiters)
  "Marks DOUBLESLASHED at point if any. "
  (interactive "P")
  (ar-th-mark 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-hide-doubleslashed-atpt (&optional no-delimiters)
  "Hides DOUBLESLASHED at point. "
  (interactive "P")
  (ar-th-hide 'doubleslashed nil nil no-delimiters))

;;;###autoload
(defun ar-show-doubleslashed-atpt (&optional no-delimiters)
  "Shows hidden DOUBLESLASHED at point. "
  (interactive "P")
  (ar-th-show 'doubleslashed nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-doubleslashed-atpt (&optional no-delimiters)
  "Alternatively hides or shows DOUBLESLASHED at point. "
  (interactive "P")
  (ar-th-hide-show 'doubleslashed nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-doubleslashed-atpt-mode (&optional no-delimiters)
  "Toggles doubleslashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-kill-doubleslashed-atpt (&optional no-delimiters)
  "Kills DOUBLESLASHED at point if any. "
  (interactive "*P")
  (ar-th-kill 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-separate-doubleslashed-atpt (&optional no-delimiters)
  "Separates DOUBLESLASHED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-triplequotedq-doubleslashed-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doubleslashed. "
  (interactive "*P")
  (ar-th-triplequotedq 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-triplequotesq-doubleslashed-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doubleslashed. "
  (interactive "*P")
  (ar-th-triplequotesq 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-triplebacktick-doubleslashed-atpt (&optional no-delimiters)
  "Triplebacktick doubleslashed at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-trim-doubleslashed-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doubleslashed no-delimiters t t))

;;;###autoload
(defun ar-left-trim-doubleslashed-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doubleslashed no-delimiters t))

;;;###autoload
(defun ar-right-trim-doubleslashed-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doubleslashed no-delimiters nil t))

;;;###autoload
(defun ar-underscore-doubleslashed-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLESLASHED. "
  (interactive "*P")
  (ar-th-underscore 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-forward-doubleslashed-atpt (&optional no-delimiters)
  "Moves forward over DOUBLESLASHED at point if any, does nothing otherwise.
Returns end position of DOUBLESLASHED "
  (interactive "P")
  (ar-th-forward 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-backward-doubleslashed-atpt (&optional no-delimiters)
  "Moves backward over DOUBLESLASHED.
Returns beginning position of DOUBLESLASHED "
  (interactive "P")
  (ar-th-backward 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-transpose-doubleslashed-atpt (&optional no-delimiters)
  "Transposes DOUBLESLASHED with DOUBLESLASHED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'doubleslashed no-delimiters))

;;;###autoload
(defun ar-sort-doubleslashed-atpt (&optional reverse)
  "Sorts doubleslasheds in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'doubleslashed reverse))

;;;###autoload
(defun ar-check-doubleslashed-atpt (&optional arg)
  "Return t if a DOUBLESLASHED at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-doubleslashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doubleslashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-doubleslashedparen-atpt (&optional no-delimiters)
  "Returns doubleslashedparen at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-bounds-of-doubleslashedparen-atpt (&optional no-delimiters)
  "Returns a list, borders of doubleslashedparen if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-doubleslashedparen-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DOUBLESLASHEDPAREN.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-doubleslashedparen-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DOUBLESLASHEDPAREN.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-beginning-of-doubleslashedparen-atpt (&optional no-delimiters)
  "Goto beginning of DOUBLESLASHEDPAREN.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-end-of-doubleslashedparen-atpt (&optional no-delimiters)
  "Goto end of DOUBLESLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-in-doubleslashedparen-p-atpt (&optional no-delimiters)
  "Returns bounds of DOUBLESLASHEDPAREN if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-length-of-doubleslashedparen-atpt (&optional no-delimiters)
  "Returns beginning of DOUBLESLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-copy-doubleslashedparen-atpt (&optional no-delimiters)
  "Returns a copy of DOUBLESLASHEDPAREN. "
  (interactive "P")
  (ar-th-copy 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-delete-doubleslashedparen-atpt (&optional no-delimiters)
  "Deletes DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-delete 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-delete-doubleslashedparen-in-region (beg end)
  "Deletes DOUBLESLASHEDPAREN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'doubleslashedparen beg end))

;;;###autoload
(defun ar-blok-doubleslashedparen-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around doubleslashedparen.
  Returns blok or nil if no DOUBLESLASHEDPAREN at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-backslashparen-doubleslashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doubleslashedparen at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-doublebackslash-doubleslashedparen-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-doubleslash-doubleslashedparen-atpt (&optional no-delimiters)
  "Puts doubled slashes around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-doubleslashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-doublebacktick-doubleslashedparen-atpt (&optional no-delimiters)
  "Provides double backticks around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-slashparen-doubleslashedparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-comment-doubleslashedparen-atpt (&optional no-delimiters)
  "Comments DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-comment 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-commatize-doubleslashedparen-atpt (&optional no-delimiters)
  "Put a comma after DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-commatize 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-quote-doubleslashedparen-atpt (&optional no-delimiters)
  "Put a singlequote before DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-quote 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-mark-doubleslashedparen-atpt (&optional no-delimiters)
  "Marks DOUBLESLASHEDPAREN at point if any. "
  (interactive "P")
  (ar-th-mark 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-hide-doubleslashedparen-atpt (&optional no-delimiters)
  "Hides DOUBLESLASHEDPAREN at point. "
  (interactive "P")
  (ar-th-hide 'doubleslashedparen nil nil no-delimiters))

;;;###autoload
(defun ar-show-doubleslashedparen-atpt (&optional no-delimiters)
  "Shows hidden DOUBLESLASHEDPAREN at point. "
  (interactive "P")
  (ar-th-show 'doubleslashedparen nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-doubleslashedparen-atpt (&optional no-delimiters)
  "Alternatively hides or shows DOUBLESLASHEDPAREN at point. "
  (interactive "P")
  (ar-th-hide-show 'doubleslashedparen nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-doubleslashedparen-atpt-mode (&optional no-delimiters)
  "Toggles doubleslashedparen-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-kill-doubleslashedparen-atpt (&optional no-delimiters)
  "Kills DOUBLESLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-kill 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-separate-doubleslashedparen-atpt (&optional no-delimiters)
  "Separates DOUBLESLASHEDPAREN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-triplequotedq-doubleslashedparen-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around doubleslashedparen. "
  (interactive "*P")
  (ar-th-triplequotedq 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-triplequotesq-doubleslashedparen-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around doubleslashedparen. "
  (interactive "*P")
  (ar-th-triplequotesq 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-triplebacktick-doubleslashedparen-atpt (&optional no-delimiters)
  "Triplebacktick doubleslashedparen at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-trim-doubleslashedparen-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'doubleslashedparen no-delimiters t t))

;;;###autoload
(defun ar-left-trim-doubleslashedparen-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'doubleslashedparen no-delimiters t))

;;;###autoload
(defun ar-right-trim-doubleslashedparen-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'doubleslashedparen no-delimiters nil t))

;;;###autoload
(defun ar-underscore-doubleslashedparen-atpt (&optional no-delimiters)
  "Put underscore char around DOUBLESLASHEDPAREN. "
  (interactive "*P")
  (ar-th-underscore 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-forward-doubleslashedparen-atpt (&optional no-delimiters)
  "Moves forward over DOUBLESLASHEDPAREN at point if any, does nothing otherwise.
Returns end position of DOUBLESLASHEDPAREN "
  (interactive "P")
  (ar-th-forward 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-backward-doubleslashedparen-atpt (&optional no-delimiters)
  "Moves backward over DOUBLESLASHEDPAREN.
Returns beginning position of DOUBLESLASHEDPAREN "
  (interactive "P")
  (ar-th-backward 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-transpose-doubleslashedparen-atpt (&optional no-delimiters)
  "Transposes DOUBLESLASHEDPAREN with DOUBLESLASHEDPAREN before point if any. "
  (interactive "*P")
  (ar-th-transpose 'doubleslashedparen no-delimiters))

;;;###autoload
(defun ar-sort-doubleslashedparen-atpt (&optional reverse)
  "Sorts doubleslashedparens in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'doubleslashedparen reverse))

;;;###autoload
(defun ar-check-doubleslashedparen-atpt (&optional arg)
  "Return t if a DOUBLESLASHEDPAREN at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-doubleslashedparen-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doubleslashedparen-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-markup-atpt (&optional no-delimiters)
  "Returns markup at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'markup no-delimiters))

;;;###autoload
(defun ar-bounds-of-markup-atpt (&optional no-delimiters)
  "Returns a list, borders of markup if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'markup no-delimiters))

;;;###autoload
(defun ar-markup-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position MARKUP.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'markup no-delimiters))

;;;###autoload
(defun ar-markup-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of MARKUP.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'markup no-delimiters))

;;;###autoload
(defun ar-beginning-of-markup-atpt (&optional no-delimiters)
  "Goto beginning of MARKUP.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'markup no-delimiters))

;;;###autoload
(defun ar-end-of-markup-atpt (&optional no-delimiters)
  "Goto end of MARKUP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'markup no-delimiters))

;;;###autoload
(defun ar-in-markup-p-atpt (&optional no-delimiters)
  "Returns bounds of MARKUP if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'markup no-delimiters))

;;;###autoload
(defun ar-length-of-markup-atpt (&optional no-delimiters)
  "Returns beginning of MARKUP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'markup no-delimiters))

;;;###autoload
(defun ar-copy-markup-atpt (&optional no-delimiters)
  "Returns a copy of MARKUP. "
  (interactive "P")
  (ar-th-copy 'markup no-delimiters))

;;;###autoload
(defun ar-delete-markup-atpt (&optional no-delimiters)
  "Deletes MARKUP at point if any. "
  (interactive "*P")
  (ar-th-delete 'markup no-delimiters))

;;;###autoload
(defun ar-delete-markup-in-region (beg end)
  "Deletes MARKUP at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'markup beg end))

;;;###autoload
(defun ar-blok-markup-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around markup.
  Returns blok or nil if no MARKUP at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'markup no-delimiters))

;;;###autoload
(defun ar-backslashparen-markup-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around markup at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'markup no-delimiters))

;;;###autoload
(defun ar-doublebackslash-markup-atpt (&optional no-delimiters)
  "Puts doubled backslashes around MARKUP at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'markup no-delimiters))

;;;###autoload
(defun ar-doubleslash-markup-atpt (&optional no-delimiters)
  "Puts doubled slashes around MARKUP at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'markup no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-markup-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around MARKUP at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'markup no-delimiters))

;;;###autoload
(defun ar-doublebacktick-markup-atpt (&optional no-delimiters)
  "Provides double backticks around MARKUP at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'markup no-delimiters))

;;;###autoload
(defun ar-slashparen-markup-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MARKUP at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'markup no-delimiters))

;;;###autoload
(defun ar-comment-markup-atpt (&optional no-delimiters)
  "Comments MARKUP at point if any. "
  (interactive "*P")
  (ar-th-comment 'markup no-delimiters))

;;;###autoload
(defun ar-commatize-markup-atpt (&optional no-delimiters)
  "Put a comma after MARKUP at point if any. "
  (interactive "*P")
  (ar-th-commatize 'markup no-delimiters))

;;;###autoload
(defun ar-quote-markup-atpt (&optional no-delimiters)
  "Put a singlequote before MARKUP at point if any. "
  (interactive "*P")
  (ar-th-quote 'markup no-delimiters))

;;;###autoload
(defun ar-mark-markup-atpt (&optional no-delimiters)
  "Marks MARKUP at point if any. "
  (interactive "P")
  (ar-th-mark 'markup no-delimiters))

;;;###autoload
(defun ar-hide-markup-atpt (&optional no-delimiters)
  "Hides MARKUP at point. "
  (interactive "P")
  (ar-th-hide 'markup nil nil no-delimiters))

;;;###autoload
(defun ar-show-markup-atpt (&optional no-delimiters)
  "Shows hidden MARKUP at point. "
  (interactive "P")
  (ar-th-show 'markup nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-markup-atpt (&optional no-delimiters)
  "Alternatively hides or shows MARKUP at point. "
  (interactive "P")
  (ar-th-hide-show 'markup nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-markup-atpt-mode (&optional no-delimiters)
  "Toggles markup-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'markup no-delimiters))

;;;###autoload
(defun ar-kill-markup-atpt (&optional no-delimiters)
  "Kills MARKUP at point if any. "
  (interactive "*P")
  (ar-th-kill 'markup no-delimiters))

;;;###autoload
(defun ar-separate-markup-atpt (&optional no-delimiters)
  "Separates MARKUP at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'markup no-delimiters))

;;;###autoload
(defun ar-triplequotedq-markup-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around markup. "
  (interactive "*P")
  (ar-th-triplequotedq 'markup no-delimiters))

;;;###autoload
(defun ar-triplequotesq-markup-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around markup. "
  (interactive "*P")
  (ar-th-triplequotesq 'markup no-delimiters))

;;;###autoload
(defun ar-triplebacktick-markup-atpt (&optional no-delimiters)
  "Triplebacktick markup at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'markup no-delimiters))

;;;###autoload
(defun ar-trim-markup-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'markup no-delimiters t t))

;;;###autoload
(defun ar-left-trim-markup-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'markup no-delimiters t))

;;;###autoload
(defun ar-right-trim-markup-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'markup no-delimiters nil t))

;;;###autoload
(defun ar-underscore-markup-atpt (&optional no-delimiters)
  "Put underscore char around MARKUP. "
  (interactive "*P")
  (ar-th-underscore 'markup no-delimiters))

;;;###autoload
(defun ar-forward-markup-atpt (&optional no-delimiters)
  "Moves forward over MARKUP at point if any, does nothing otherwise.
Returns end position of MARKUP "
  (interactive "P")
  (ar-th-forward 'markup no-delimiters))

;;;###autoload
(defun ar-backward-markup-atpt (&optional no-delimiters)
  "Moves backward over MARKUP.
Returns beginning position of MARKUP "
  (interactive "P")
  (ar-th-backward 'markup no-delimiters))

;;;###autoload
(defun ar-transpose-markup-atpt (&optional no-delimiters)
  "Transposes MARKUP with MARKUP before point if any. "
  (interactive "*P")
  (ar-th-transpose 'markup no-delimiters))

;;;###autoload
(defun ar-sort-markup-atpt (&optional reverse)
  "Sorts markups in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'markup reverse))

;;;###autoload
(defun ar-check-markup-atpt (&optional arg)
  "Return t if a MARKUP at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-markup-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-markup-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-mldata-atpt (&optional no-delimiters)
  "Returns mldata at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'mldata no-delimiters))

;;;###autoload
(defun ar-bounds-of-mldata-atpt (&optional no-delimiters)
  "Returns a list, borders of mldata if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'mldata no-delimiters))

;;;###autoload
(defun ar-mldata-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position MLDATA.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'mldata no-delimiters))

;;;###autoload
(defun ar-mldata-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of MLDATA.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'mldata no-delimiters))

;;;###autoload
(defun ar-beginning-of-mldata-atpt (&optional no-delimiters)
  "Goto beginning of MLDATA.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'mldata no-delimiters))

;;;###autoload
(defun ar-end-of-mldata-atpt (&optional no-delimiters)
  "Goto end of MLDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'mldata no-delimiters))

;;;###autoload
(defun ar-in-mldata-p-atpt (&optional no-delimiters)
  "Returns bounds of MLDATA if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'mldata no-delimiters))

;;;###autoload
(defun ar-length-of-mldata-atpt (&optional no-delimiters)
  "Returns beginning of MLDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'mldata no-delimiters))

;;;###autoload
(defun ar-copy-mldata-atpt (&optional no-delimiters)
  "Returns a copy of MLDATA. "
  (interactive "P")
  (ar-th-copy 'mldata no-delimiters))

;;;###autoload
(defun ar-delete-mldata-atpt (&optional no-delimiters)
  "Deletes MLDATA at point if any. "
  (interactive "*P")
  (ar-th-delete 'mldata no-delimiters))

;;;###autoload
(defun ar-delete-mldata-in-region (beg end)
  "Deletes MLDATA at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'mldata beg end))

;;;###autoload
(defun ar-blok-mldata-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around mldata.
  Returns blok or nil if no MLDATA at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'mldata no-delimiters))

;;;###autoload
(defun ar-backslashparen-mldata-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around mldata at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'mldata no-delimiters))

;;;###autoload
(defun ar-doublebackslash-mldata-atpt (&optional no-delimiters)
  "Puts doubled backslashes around MLDATA at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'mldata no-delimiters))

;;;###autoload
(defun ar-doubleslash-mldata-atpt (&optional no-delimiters)
  "Puts doubled slashes around MLDATA at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'mldata no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-mldata-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around MLDATA at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'mldata no-delimiters))

;;;###autoload
(defun ar-doublebacktick-mldata-atpt (&optional no-delimiters)
  "Provides double backticks around MLDATA at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'mldata no-delimiters))

;;;###autoload
(defun ar-slashparen-mldata-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MLDATA at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'mldata no-delimiters))

;;;###autoload
(defun ar-comment-mldata-atpt (&optional no-delimiters)
  "Comments MLDATA at point if any. "
  (interactive "*P")
  (ar-th-comment 'mldata no-delimiters))

;;;###autoload
(defun ar-commatize-mldata-atpt (&optional no-delimiters)
  "Put a comma after MLDATA at point if any. "
  (interactive "*P")
  (ar-th-commatize 'mldata no-delimiters))

;;;###autoload
(defun ar-quote-mldata-atpt (&optional no-delimiters)
  "Put a singlequote before MLDATA at point if any. "
  (interactive "*P")
  (ar-th-quote 'mldata no-delimiters))

;;;###autoload
(defun ar-mark-mldata-atpt (&optional no-delimiters)
  "Marks MLDATA at point if any. "
  (interactive "P")
  (ar-th-mark 'mldata no-delimiters))

;;;###autoload
(defun ar-hide-mldata-atpt (&optional no-delimiters)
  "Hides MLDATA at point. "
  (interactive "P")
  (ar-th-hide 'mldata nil nil no-delimiters))

;;;###autoload
(defun ar-show-mldata-atpt (&optional no-delimiters)
  "Shows hidden MLDATA at point. "
  (interactive "P")
  (ar-th-show 'mldata nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-mldata-atpt (&optional no-delimiters)
  "Alternatively hides or shows MLDATA at point. "
  (interactive "P")
  (ar-th-hide-show 'mldata nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-mldata-atpt-mode (&optional no-delimiters)
  "Toggles mldata-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'mldata no-delimiters))

;;;###autoload
(defun ar-kill-mldata-atpt (&optional no-delimiters)
  "Kills MLDATA at point if any. "
  (interactive "*P")
  (ar-th-kill 'mldata no-delimiters))

;;;###autoload
(defun ar-separate-mldata-atpt (&optional no-delimiters)
  "Separates MLDATA at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'mldata no-delimiters))

;;;###autoload
(defun ar-triplequotedq-mldata-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around mldata. "
  (interactive "*P")
  (ar-th-triplequotedq 'mldata no-delimiters))

;;;###autoload
(defun ar-triplequotesq-mldata-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around mldata. "
  (interactive "*P")
  (ar-th-triplequotesq 'mldata no-delimiters))

;;;###autoload
(defun ar-triplebacktick-mldata-atpt (&optional no-delimiters)
  "Triplebacktick mldata at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'mldata no-delimiters))

;;;###autoload
(defun ar-trim-mldata-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'mldata no-delimiters t t))

;;;###autoload
(defun ar-left-trim-mldata-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'mldata no-delimiters t))

;;;###autoload
(defun ar-right-trim-mldata-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'mldata no-delimiters nil t))

;;;###autoload
(defun ar-underscore-mldata-atpt (&optional no-delimiters)
  "Put underscore char around MLDATA. "
  (interactive "*P")
  (ar-th-underscore 'mldata no-delimiters))

;;;###autoload
(defun ar-forward-mldata-atpt (&optional no-delimiters)
  "Moves forward over MLDATA at point if any, does nothing otherwise.
Returns end position of MLDATA "
  (interactive "P")
  (ar-th-forward 'mldata no-delimiters))

;;;###autoload
(defun ar-backward-mldata-atpt (&optional no-delimiters)
  "Moves backward over MLDATA.
Returns beginning position of MLDATA "
  (interactive "P")
  (ar-th-backward 'mldata no-delimiters))

;;;###autoload
(defun ar-transpose-mldata-atpt (&optional no-delimiters)
  "Transposes MLDATA with MLDATA before point if any. "
  (interactive "*P")
  (ar-th-transpose 'mldata no-delimiters))

;;;###autoload
(defun ar-sort-mldata-atpt (&optional reverse)
  "Sorts mldatas in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'mldata reverse))

;;;###autoload
(defun ar-check-mldata-atpt (&optional arg)
  "Return t if a MLDATA at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-mldata-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-mldata-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-mlattribut-atpt (&optional no-delimiters)
  "Returns mlattribut at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'mlattribut no-delimiters))

;;;###autoload
(defun ar-bounds-of-mlattribut-atpt (&optional no-delimiters)
  "Returns a list, borders of mlattribut if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'mlattribut no-delimiters))

;;;###autoload
(defun ar-mlattribut-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position MLATTRIBUT.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'mlattribut no-delimiters))

;;;###autoload
(defun ar-mlattribut-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of MLATTRIBUT.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'mlattribut no-delimiters))

;;;###autoload
(defun ar-beginning-of-mlattribut-atpt (&optional no-delimiters)
  "Goto beginning of MLATTRIBUT.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'mlattribut no-delimiters))

;;;###autoload
(defun ar-end-of-mlattribut-atpt (&optional no-delimiters)
  "Goto end of MLATTRIBUT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'mlattribut no-delimiters))

;;;###autoload
(defun ar-in-mlattribut-p-atpt (&optional no-delimiters)
  "Returns bounds of MLATTRIBUT if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'mlattribut no-delimiters))

;;;###autoload
(defun ar-length-of-mlattribut-atpt (&optional no-delimiters)
  "Returns beginning of MLATTRIBUT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'mlattribut no-delimiters))

;;;###autoload
(defun ar-copy-mlattribut-atpt (&optional no-delimiters)
  "Returns a copy of MLATTRIBUT. "
  (interactive "P")
  (ar-th-copy 'mlattribut no-delimiters))

;;;###autoload
(defun ar-delete-mlattribut-atpt (&optional no-delimiters)
  "Deletes MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-delete 'mlattribut no-delimiters))

;;;###autoload
(defun ar-delete-mlattribut-in-region (beg end)
  "Deletes MLATTRIBUT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'mlattribut beg end))

;;;###autoload
(defun ar-blok-mlattribut-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around mlattribut.
  Returns blok or nil if no MLATTRIBUT at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'mlattribut no-delimiters))

;;;###autoload
(defun ar-backslashparen-mlattribut-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around mlattribut at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'mlattribut no-delimiters))

;;;###autoload
(defun ar-doublebackslash-mlattribut-atpt (&optional no-delimiters)
  "Puts doubled backslashes around MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'mlattribut no-delimiters))

;;;###autoload
(defun ar-doubleslash-mlattribut-atpt (&optional no-delimiters)
  "Puts doubled slashes around MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'mlattribut no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-mlattribut-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'mlattribut no-delimiters))

;;;###autoload
(defun ar-doublebacktick-mlattribut-atpt (&optional no-delimiters)
  "Provides double backticks around MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'mlattribut no-delimiters))

;;;###autoload
(defun ar-slashparen-mlattribut-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'mlattribut no-delimiters))

;;;###autoload
(defun ar-comment-mlattribut-atpt (&optional no-delimiters)
  "Comments MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-comment 'mlattribut no-delimiters))

;;;###autoload
(defun ar-commatize-mlattribut-atpt (&optional no-delimiters)
  "Put a comma after MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-commatize 'mlattribut no-delimiters))

;;;###autoload
(defun ar-quote-mlattribut-atpt (&optional no-delimiters)
  "Put a singlequote before MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-quote 'mlattribut no-delimiters))

;;;###autoload
(defun ar-mark-mlattribut-atpt (&optional no-delimiters)
  "Marks MLATTRIBUT at point if any. "
  (interactive "P")
  (ar-th-mark 'mlattribut no-delimiters))

;;;###autoload
(defun ar-hide-mlattribut-atpt (&optional no-delimiters)
  "Hides MLATTRIBUT at point. "
  (interactive "P")
  (ar-th-hide 'mlattribut nil nil no-delimiters))

;;;###autoload
(defun ar-show-mlattribut-atpt (&optional no-delimiters)
  "Shows hidden MLATTRIBUT at point. "
  (interactive "P")
  (ar-th-show 'mlattribut nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-mlattribut-atpt (&optional no-delimiters)
  "Alternatively hides or shows MLATTRIBUT at point. "
  (interactive "P")
  (ar-th-hide-show 'mlattribut nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-mlattribut-atpt-mode (&optional no-delimiters)
  "Toggles mlattribut-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'mlattribut no-delimiters))

;;;###autoload
(defun ar-kill-mlattribut-atpt (&optional no-delimiters)
  "Kills MLATTRIBUT at point if any. "
  (interactive "*P")
  (ar-th-kill 'mlattribut no-delimiters))

;;;###autoload
(defun ar-separate-mlattribut-atpt (&optional no-delimiters)
  "Separates MLATTRIBUT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'mlattribut no-delimiters))

;;;###autoload
(defun ar-triplequotedq-mlattribut-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around mlattribut. "
  (interactive "*P")
  (ar-th-triplequotedq 'mlattribut no-delimiters))

;;;###autoload
(defun ar-triplequotesq-mlattribut-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around mlattribut. "
  (interactive "*P")
  (ar-th-triplequotesq 'mlattribut no-delimiters))

;;;###autoload
(defun ar-triplebacktick-mlattribut-atpt (&optional no-delimiters)
  "Triplebacktick mlattribut at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'mlattribut no-delimiters))

;;;###autoload
(defun ar-trim-mlattribut-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'mlattribut no-delimiters t t))

;;;###autoload
(defun ar-left-trim-mlattribut-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'mlattribut no-delimiters t))

;;;###autoload
(defun ar-right-trim-mlattribut-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'mlattribut no-delimiters nil t))

;;;###autoload
(defun ar-underscore-mlattribut-atpt (&optional no-delimiters)
  "Put underscore char around MLATTRIBUT. "
  (interactive "*P")
  (ar-th-underscore 'mlattribut no-delimiters))

;;;###autoload
(defun ar-forward-mlattribut-atpt (&optional no-delimiters)
  "Moves forward over MLATTRIBUT at point if any, does nothing otherwise.
Returns end position of MLATTRIBUT "
  (interactive "P")
  (ar-th-forward 'mlattribut no-delimiters))

;;;###autoload
(defun ar-backward-mlattribut-atpt (&optional no-delimiters)
  "Moves backward over MLATTRIBUT.
Returns beginning position of MLATTRIBUT "
  (interactive "P")
  (ar-th-backward 'mlattribut no-delimiters))

;;;###autoload
(defun ar-transpose-mlattribut-atpt (&optional no-delimiters)
  "Transposes MLATTRIBUT with MLATTRIBUT before point if any. "
  (interactive "*P")
  (ar-th-transpose 'mlattribut no-delimiters))

;;;###autoload
(defun ar-sort-mlattribut-atpt (&optional reverse)
  "Sorts mlattributs in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'mlattribut reverse))

;;;###autoload
(defun ar-check-mlattribut-atpt (&optional arg)
  "Return t if a MLATTRIBUT at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-mlattribut-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-mlattribut-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-mltag-atpt (&optional no-delimiters)
  "Returns mltag at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'mltag no-delimiters))

;;;###autoload
(defun ar-bounds-of-mltag-atpt (&optional no-delimiters)
  "Returns a list, borders of mltag if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'mltag no-delimiters))

;;;###autoload
(defun ar-mltag-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position MLTAG.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'mltag no-delimiters))

;;;###autoload
(defun ar-mltag-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of MLTAG.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'mltag no-delimiters))

;;;###autoload
(defun ar-beginning-of-mltag-atpt (&optional no-delimiters)
  "Goto beginning of MLTAG.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'mltag no-delimiters))

;;;###autoload
(defun ar-end-of-mltag-atpt (&optional no-delimiters)
  "Goto end of MLTAG at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'mltag no-delimiters))

;;;###autoload
(defun ar-in-mltag-p-atpt (&optional no-delimiters)
  "Returns bounds of MLTAG if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'mltag no-delimiters))

;;;###autoload
(defun ar-length-of-mltag-atpt (&optional no-delimiters)
  "Returns beginning of MLTAG at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'mltag no-delimiters))

;;;###autoload
(defun ar-copy-mltag-atpt (&optional no-delimiters)
  "Returns a copy of MLTAG. "
  (interactive "P")
  (ar-th-copy 'mltag no-delimiters))

;;;###autoload
(defun ar-delete-mltag-atpt (&optional no-delimiters)
  "Deletes MLTAG at point if any. "
  (interactive "*P")
  (ar-th-delete 'mltag no-delimiters))

;;;###autoload
(defun ar-delete-mltag-in-region (beg end)
  "Deletes MLTAG at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'mltag beg end))

;;;###autoload
(defun ar-blok-mltag-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around mltag.
  Returns blok or nil if no MLTAG at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'mltag no-delimiters))

;;;###autoload
(defun ar-backslashparen-mltag-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around mltag at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'mltag no-delimiters))

;;;###autoload
(defun ar-doublebackslash-mltag-atpt (&optional no-delimiters)
  "Puts doubled backslashes around MLTAG at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'mltag no-delimiters))

;;;###autoload
(defun ar-doubleslash-mltag-atpt (&optional no-delimiters)
  "Puts doubled slashes around MLTAG at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'mltag no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-mltag-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around MLTAG at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'mltag no-delimiters))

;;;###autoload
(defun ar-doublebacktick-mltag-atpt (&optional no-delimiters)
  "Provides double backticks around MLTAG at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'mltag no-delimiters))

;;;###autoload
(defun ar-slashparen-mltag-atpt (&optional no-delimiters)
  "Provides slashed parentheses around MLTAG at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'mltag no-delimiters))

;;;###autoload
(defun ar-comment-mltag-atpt (&optional no-delimiters)
  "Comments MLTAG at point if any. "
  (interactive "*P")
  (ar-th-comment 'mltag no-delimiters))

;;;###autoload
(defun ar-commatize-mltag-atpt (&optional no-delimiters)
  "Put a comma after MLTAG at point if any. "
  (interactive "*P")
  (ar-th-commatize 'mltag no-delimiters))

;;;###autoload
(defun ar-quote-mltag-atpt (&optional no-delimiters)
  "Put a singlequote before MLTAG at point if any. "
  (interactive "*P")
  (ar-th-quote 'mltag no-delimiters))

;;;###autoload
(defun ar-mark-mltag-atpt (&optional no-delimiters)
  "Marks MLTAG at point if any. "
  (interactive "P")
  (ar-th-mark 'mltag no-delimiters))

;;;###autoload
(defun ar-hide-mltag-atpt (&optional no-delimiters)
  "Hides MLTAG at point. "
  (interactive "P")
  (ar-th-hide 'mltag nil nil no-delimiters))

;;;###autoload
(defun ar-show-mltag-atpt (&optional no-delimiters)
  "Shows hidden MLTAG at point. "
  (interactive "P")
  (ar-th-show 'mltag nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-mltag-atpt (&optional no-delimiters)
  "Alternatively hides or shows MLTAG at point. "
  (interactive "P")
  (ar-th-hide-show 'mltag nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-mltag-atpt-mode (&optional no-delimiters)
  "Toggles mltag-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'mltag no-delimiters))

;;;###autoload
(defun ar-kill-mltag-atpt (&optional no-delimiters)
  "Kills MLTAG at point if any. "
  (interactive "*P")
  (ar-th-kill 'mltag no-delimiters))

;;;###autoload
(defun ar-separate-mltag-atpt (&optional no-delimiters)
  "Separates MLTAG at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'mltag no-delimiters))

;;;###autoload
(defun ar-triplequotedq-mltag-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around mltag. "
  (interactive "*P")
  (ar-th-triplequotedq 'mltag no-delimiters))

;;;###autoload
(defun ar-triplequotesq-mltag-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around mltag. "
  (interactive "*P")
  (ar-th-triplequotesq 'mltag no-delimiters))

;;;###autoload
(defun ar-triplebacktick-mltag-atpt (&optional no-delimiters)
  "Triplebacktick mltag at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'mltag no-delimiters))

;;;###autoload
(defun ar-trim-mltag-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'mltag no-delimiters t t))

;;;###autoload
(defun ar-left-trim-mltag-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'mltag no-delimiters t))

;;;###autoload
(defun ar-right-trim-mltag-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'mltag no-delimiters nil t))

;;;###autoload
(defun ar-underscore-mltag-atpt (&optional no-delimiters)
  "Put underscore char around MLTAG. "
  (interactive "*P")
  (ar-th-underscore 'mltag no-delimiters))

;;;###autoload
(defun ar-forward-mltag-atpt (&optional no-delimiters)
  "Moves forward over MLTAG at point if any, does nothing otherwise.
Returns end position of MLTAG "
  (interactive "P")
  (ar-th-forward 'mltag no-delimiters))

;;;###autoload
(defun ar-backward-mltag-atpt (&optional no-delimiters)
  "Moves backward over MLTAG.
Returns beginning position of MLTAG "
  (interactive "P")
  (ar-th-backward 'mltag no-delimiters))

;;;###autoload
(defun ar-transpose-mltag-atpt (&optional no-delimiters)
  "Transposes MLTAG with MLTAG before point if any. "
  (interactive "*P")
  (ar-th-transpose 'mltag no-delimiters))

;;;###autoload
(defun ar-sort-mltag-atpt (&optional reverse)
  "Sorts mltags in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'mltag reverse))

;;;###autoload
(defun ar-check-mltag-atpt (&optional arg)
  "Return t if a MLTAG at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-mltag-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-mltag-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-slashedparen-atpt (&optional no-delimiters)
  "Returns slashedparen at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'slashedparen no-delimiters))

;;;###autoload
(defun ar-bounds-of-slashedparen-atpt (&optional no-delimiters)
  "Returns a list, borders of slashedparen if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'slashedparen no-delimiters))

;;;###autoload
(defun ar-slashedparen-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position SLASHEDPAREN.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'slashedparen no-delimiters))

;;;###autoload
(defun ar-slashedparen-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of SLASHEDPAREN.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'slashedparen no-delimiters))

;;;###autoload
(defun ar-beginning-of-slashedparen-atpt (&optional no-delimiters)
  "Goto beginning of SLASHEDPAREN.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'slashedparen no-delimiters))

;;;###autoload
(defun ar-end-of-slashedparen-atpt (&optional no-delimiters)
  "Goto end of SLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'slashedparen no-delimiters))

;;;###autoload
(defun ar-in-slashedparen-p-atpt (&optional no-delimiters)
  "Returns bounds of SLASHEDPAREN if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'slashedparen no-delimiters))

;;;###autoload
(defun ar-length-of-slashedparen-atpt (&optional no-delimiters)
  "Returns beginning of SLASHEDPAREN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'slashedparen no-delimiters))

;;;###autoload
(defun ar-copy-slashedparen-atpt (&optional no-delimiters)
  "Returns a copy of SLASHEDPAREN. "
  (interactive "P")
  (ar-th-copy 'slashedparen no-delimiters))

;;;###autoload
(defun ar-delete-slashedparen-atpt (&optional no-delimiters)
  "Deletes SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-delete 'slashedparen no-delimiters))

;;;###autoload
(defun ar-delete-slashedparen-in-region (beg end)
  "Deletes SLASHEDPAREN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'slashedparen beg end))

;;;###autoload
(defun ar-blok-slashedparen-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around slashedparen.
  Returns blok or nil if no SLASHEDPAREN at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'slashedparen no-delimiters))

;;;###autoload
(defun ar-backslashparen-slashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around slashedparen at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'slashedparen no-delimiters))

;;;###autoload
(defun ar-doublebackslash-slashedparen-atpt (&optional no-delimiters)
  "Puts doubled backslashes around SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'slashedparen no-delimiters))

;;;###autoload
(defun ar-doubleslash-slashedparen-atpt (&optional no-delimiters)
  "Puts doubled slashes around SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'slashedparen no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-slashedparen-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'slashedparen no-delimiters))

;;;###autoload
(defun ar-doublebacktick-slashedparen-atpt (&optional no-delimiters)
  "Provides double backticks around SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'slashedparen no-delimiters))

;;;###autoload
(defun ar-slashparen-slashedparen-atpt (&optional no-delimiters)
  "Provides slashed parentheses around SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'slashedparen no-delimiters))

;;;###autoload
(defun ar-comment-slashedparen-atpt (&optional no-delimiters)
  "Comments SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-comment 'slashedparen no-delimiters))

;;;###autoload
(defun ar-commatize-slashedparen-atpt (&optional no-delimiters)
  "Put a comma after SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-commatize 'slashedparen no-delimiters))

;;;###autoload
(defun ar-quote-slashedparen-atpt (&optional no-delimiters)
  "Put a singlequote before SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-quote 'slashedparen no-delimiters))

;;;###autoload
(defun ar-mark-slashedparen-atpt (&optional no-delimiters)
  "Marks SLASHEDPAREN at point if any. "
  (interactive "P")
  (ar-th-mark 'slashedparen no-delimiters))

;;;###autoload
(defun ar-hide-slashedparen-atpt (&optional no-delimiters)
  "Hides SLASHEDPAREN at point. "
  (interactive "P")
  (ar-th-hide 'slashedparen nil nil no-delimiters))

;;;###autoload
(defun ar-show-slashedparen-atpt (&optional no-delimiters)
  "Shows hidden SLASHEDPAREN at point. "
  (interactive "P")
  (ar-th-show 'slashedparen nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-slashedparen-atpt (&optional no-delimiters)
  "Alternatively hides or shows SLASHEDPAREN at point. "
  (interactive "P")
  (ar-th-hide-show 'slashedparen nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-slashedparen-atpt-mode (&optional no-delimiters)
  "Toggles slashedparen-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'slashedparen no-delimiters))

;;;###autoload
(defun ar-kill-slashedparen-atpt (&optional no-delimiters)
  "Kills SLASHEDPAREN at point if any. "
  (interactive "*P")
  (ar-th-kill 'slashedparen no-delimiters))

;;;###autoload
(defun ar-separate-slashedparen-atpt (&optional no-delimiters)
  "Separates SLASHEDPAREN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'slashedparen no-delimiters))

;;;###autoload
(defun ar-triplequotedq-slashedparen-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around slashedparen. "
  (interactive "*P")
  (ar-th-triplequotedq 'slashedparen no-delimiters))

;;;###autoload
(defun ar-triplequotesq-slashedparen-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around slashedparen. "
  (interactive "*P")
  (ar-th-triplequotesq 'slashedparen no-delimiters))

;;;###autoload
(defun ar-triplebacktick-slashedparen-atpt (&optional no-delimiters)
  "Triplebacktick slashedparen at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'slashedparen no-delimiters))

;;;###autoload
(defun ar-trim-slashedparen-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'slashedparen no-delimiters t t))

;;;###autoload
(defun ar-left-trim-slashedparen-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'slashedparen no-delimiters t))

;;;###autoload
(defun ar-right-trim-slashedparen-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'slashedparen no-delimiters nil t))

;;;###autoload
(defun ar-underscore-slashedparen-atpt (&optional no-delimiters)
  "Put underscore char around SLASHEDPAREN. "
  (interactive "*P")
  (ar-th-underscore 'slashedparen no-delimiters))

;;;###autoload
(defun ar-forward-slashedparen-atpt (&optional no-delimiters)
  "Moves forward over SLASHEDPAREN at point if any, does nothing otherwise.
Returns end position of SLASHEDPAREN "
  (interactive "P")
  (ar-th-forward 'slashedparen no-delimiters))

;;;###autoload
(defun ar-backward-slashedparen-atpt (&optional no-delimiters)
  "Moves backward over SLASHEDPAREN.
Returns beginning position of SLASHEDPAREN "
  (interactive "P")
  (ar-th-backward 'slashedparen no-delimiters))

;;;###autoload
(defun ar-transpose-slashedparen-atpt (&optional no-delimiters)
  "Transposes SLASHEDPAREN with SLASHEDPAREN before point if any. "
  (interactive "*P")
  (ar-th-transpose 'slashedparen no-delimiters))

;;;###autoload
(defun ar-sort-slashedparen-atpt (&optional reverse)
  "Sorts slashedparens in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'slashedparen reverse))

;;;###autoload
(defun ar-check-slashedparen-atpt (&optional arg)
  "Return t if a SLASHEDPAREN at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-slashedparen-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-slashedparen-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-tabledata-atpt (&optional no-delimiters)
  "Returns tabledata at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'tabledata no-delimiters))

;;;###autoload
(defun ar-bounds-of-tabledata-atpt (&optional no-delimiters)
  "Returns a list, borders of tabledata if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'tabledata no-delimiters))

;;;###autoload
(defun ar-tabledata-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position TABLEDATA.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'tabledata no-delimiters))

;;;###autoload
(defun ar-tabledata-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of TABLEDATA.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'tabledata no-delimiters))

;;;###autoload
(defun ar-beginning-of-tabledata-atpt (&optional no-delimiters)
  "Goto beginning of TABLEDATA.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'tabledata no-delimiters))

;;;###autoload
(defun ar-end-of-tabledata-atpt (&optional no-delimiters)
  "Goto end of TABLEDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'tabledata no-delimiters))

;;;###autoload
(defun ar-in-tabledata-p-atpt (&optional no-delimiters)
  "Returns bounds of TABLEDATA if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'tabledata no-delimiters))

;;;###autoload
(defun ar-length-of-tabledata-atpt (&optional no-delimiters)
  "Returns beginning of TABLEDATA at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'tabledata no-delimiters))

;;;###autoload
(defun ar-copy-tabledata-atpt (&optional no-delimiters)
  "Returns a copy of TABLEDATA. "
  (interactive "P")
  (ar-th-copy 'tabledata no-delimiters))

;;;###autoload
(defun ar-delete-tabledata-atpt (&optional no-delimiters)
  "Deletes TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-delete 'tabledata no-delimiters))

;;;###autoload
(defun ar-delete-tabledata-in-region (beg end)
  "Deletes TABLEDATA at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'tabledata beg end))

;;;###autoload
(defun ar-blok-tabledata-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around tabledata.
  Returns blok or nil if no TABLEDATA at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'tabledata no-delimiters))

;;;###autoload
(defun ar-backslashparen-tabledata-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around tabledata at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'tabledata no-delimiters))

;;;###autoload
(defun ar-doublebackslash-tabledata-atpt (&optional no-delimiters)
  "Puts doubled backslashes around TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'tabledata no-delimiters))

;;;###autoload
(defun ar-doubleslash-tabledata-atpt (&optional no-delimiters)
  "Puts doubled slashes around TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'tabledata no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-tabledata-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'tabledata no-delimiters))

;;;###autoload
(defun ar-doublebacktick-tabledata-atpt (&optional no-delimiters)
  "Provides double backticks around TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'tabledata no-delimiters))

;;;###autoload
(defun ar-slashparen-tabledata-atpt (&optional no-delimiters)
  "Provides slashed parentheses around TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'tabledata no-delimiters))

;;;###autoload
(defun ar-comment-tabledata-atpt (&optional no-delimiters)
  "Comments TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-comment 'tabledata no-delimiters))

;;;###autoload
(defun ar-commatize-tabledata-atpt (&optional no-delimiters)
  "Put a comma after TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-commatize 'tabledata no-delimiters))

;;;###autoload
(defun ar-quote-tabledata-atpt (&optional no-delimiters)
  "Put a singlequote before TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-quote 'tabledata no-delimiters))

;;;###autoload
(defun ar-mark-tabledata-atpt (&optional no-delimiters)
  "Marks TABLEDATA at point if any. "
  (interactive "P")
  (ar-th-mark 'tabledata no-delimiters))

;;;###autoload
(defun ar-hide-tabledata-atpt (&optional no-delimiters)
  "Hides TABLEDATA at point. "
  (interactive "P")
  (ar-th-hide 'tabledata nil nil no-delimiters))

;;;###autoload
(defun ar-show-tabledata-atpt (&optional no-delimiters)
  "Shows hidden TABLEDATA at point. "
  (interactive "P")
  (ar-th-show 'tabledata nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-tabledata-atpt (&optional no-delimiters)
  "Alternatively hides or shows TABLEDATA at point. "
  (interactive "P")
  (ar-th-hide-show 'tabledata nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-tabledata-atpt-mode (&optional no-delimiters)
  "Toggles tabledata-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'tabledata no-delimiters))

;;;###autoload
(defun ar-kill-tabledata-atpt (&optional no-delimiters)
  "Kills TABLEDATA at point if any. "
  (interactive "*P")
  (ar-th-kill 'tabledata no-delimiters))

;;;###autoload
(defun ar-separate-tabledata-atpt (&optional no-delimiters)
  "Separates TABLEDATA at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'tabledata no-delimiters))

;;;###autoload
(defun ar-triplequotedq-tabledata-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around tabledata. "
  (interactive "*P")
  (ar-th-triplequotedq 'tabledata no-delimiters))

;;;###autoload
(defun ar-triplequotesq-tabledata-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around tabledata. "
  (interactive "*P")
  (ar-th-triplequotesq 'tabledata no-delimiters))

;;;###autoload
(defun ar-triplebacktick-tabledata-atpt (&optional no-delimiters)
  "Triplebacktick tabledata at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'tabledata no-delimiters))

;;;###autoload
(defun ar-trim-tabledata-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'tabledata no-delimiters t t))

;;;###autoload
(defun ar-left-trim-tabledata-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'tabledata no-delimiters t))

;;;###autoload
(defun ar-right-trim-tabledata-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'tabledata no-delimiters nil t))

;;;###autoload
(defun ar-underscore-tabledata-atpt (&optional no-delimiters)
  "Put underscore char around TABLEDATA. "
  (interactive "*P")
  (ar-th-underscore 'tabledata no-delimiters))

;;;###autoload
(defun ar-forward-tabledata-atpt (&optional no-delimiters)
  "Moves forward over TABLEDATA at point if any, does nothing otherwise.
Returns end position of TABLEDATA "
  (interactive "P")
  (ar-th-forward 'tabledata no-delimiters))

;;;###autoload
(defun ar-backward-tabledata-atpt (&optional no-delimiters)
  "Moves backward over TABLEDATA.
Returns beginning position of TABLEDATA "
  (interactive "P")
  (ar-th-backward 'tabledata no-delimiters))

;;;###autoload
(defun ar-transpose-tabledata-atpt (&optional no-delimiters)
  "Transposes TABLEDATA with TABLEDATA before point if any. "
  (interactive "*P")
  (ar-th-transpose 'tabledata no-delimiters))

;;;###autoload
(defun ar-sort-tabledata-atpt (&optional reverse)
  "Sorts tabledatas in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'tabledata reverse))

;;;###autoload
(defun ar-check-tabledata-atpt (&optional arg)
  "Return t if a TABLEDATA at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-tabledata-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-tabledata-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-triplebackticked-atpt (&optional no-delimiters)
  "Returns triplebackticked at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-bounds-of-triplebackticked-atpt (&optional no-delimiters)
  "Returns a list, borders of triplebackticked if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-triplebackticked-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position TRIPLEBACKTICKED.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-triplebackticked-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of TRIPLEBACKTICKED.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-beginning-of-triplebackticked-atpt (&optional no-delimiters)
  "Goto beginning of TRIPLEBACKTICKED.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-end-of-triplebackticked-atpt (&optional no-delimiters)
  "Goto end of TRIPLEBACKTICKED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-in-triplebackticked-p-atpt (&optional no-delimiters)
  "Returns bounds of TRIPLEBACKTICKED if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-length-of-triplebackticked-atpt (&optional no-delimiters)
  "Returns beginning of TRIPLEBACKTICKED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-copy-triplebackticked-atpt (&optional no-delimiters)
  "Returns a copy of TRIPLEBACKTICKED. "
  (interactive "P")
  (ar-th-copy 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-delete-triplebackticked-atpt (&optional no-delimiters)
  "Deletes TRIPLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-delete 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-delete-triplebackticked-in-region (beg end)
  "Deletes TRIPLEBACKTICKED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'triplebackticked beg end))

;;;###autoload
(defun ar-blok-triplebackticked-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around triplebackticked.
  Returns blok or nil if no TRIPLEBACKTICKED at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-backslashparen-triplebackticked-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around triplebackticked at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-doublebackslash-triplebackticked-atpt (&optional no-delimiters)
  "Puts doubled backslashes around TRIPLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-doubleslash-triplebackticked-atpt (&optional no-delimiters)
  "Puts doubled slashes around TRIPLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-triplebackticked-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around TRIPLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-doublebacktick-triplebackticked-atpt (&optional no-delimiters)
  "Provides double backticks around TRIPLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-slashparen-triplebackticked-atpt (&optional no-delimiters)
  "Provides slashed parentheses around TRIPLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-comment-triplebackticked-atpt (&optional no-delimiters)
  "Comments TRIPLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-comment 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-commatize-triplebackticked-atpt (&optional no-delimiters)
  "Put a comma after TRIPLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-commatize 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-quote-triplebackticked-atpt (&optional no-delimiters)
  "Put a singlequote before TRIPLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-quote 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-mark-triplebackticked-atpt (&optional no-delimiters)
  "Marks TRIPLEBACKTICKED at point if any. "
  (interactive "P")
  (ar-th-mark 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-hide-triplebackticked-atpt (&optional no-delimiters)
  "Hides TRIPLEBACKTICKED at point. "
  (interactive "P")
  (ar-th-hide 'triplebackticked nil nil no-delimiters))

;;;###autoload
(defun ar-show-triplebackticked-atpt (&optional no-delimiters)
  "Shows hidden TRIPLEBACKTICKED at point. "
  (interactive "P")
  (ar-th-show 'triplebackticked nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-triplebackticked-atpt (&optional no-delimiters)
  "Alternatively hides or shows TRIPLEBACKTICKED at point. "
  (interactive "P")
  (ar-th-hide-show 'triplebackticked nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-triplebackticked-atpt-mode (&optional no-delimiters)
  "Toggles triplebackticked-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-kill-triplebackticked-atpt (&optional no-delimiters)
  "Kills TRIPLEBACKTICKED at point if any. "
  (interactive "*P")
  (ar-th-kill 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-separate-triplebackticked-atpt (&optional no-delimiters)
  "Separates TRIPLEBACKTICKED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-triplequotedq-triplebackticked-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around triplebackticked. "
  (interactive "*P")
  (ar-th-triplequotedq 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-triplequotesq-triplebackticked-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around triplebackticked. "
  (interactive "*P")
  (ar-th-triplequotesq 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-triplebacktick-triplebackticked-atpt (&optional no-delimiters)
  "Triplebacktick triplebackticked at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-trim-triplebackticked-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'triplebackticked no-delimiters t t))

;;;###autoload
(defun ar-left-trim-triplebackticked-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'triplebackticked no-delimiters t))

;;;###autoload
(defun ar-right-trim-triplebackticked-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'triplebackticked no-delimiters nil t))

;;;###autoload
(defun ar-underscore-triplebackticked-atpt (&optional no-delimiters)
  "Put underscore char around TRIPLEBACKTICKED. "
  (interactive "*P")
  (ar-th-underscore 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-forward-triplebackticked-atpt (&optional no-delimiters)
  "Moves forward over TRIPLEBACKTICKED at point if any, does nothing otherwise.
Returns end position of TRIPLEBACKTICKED "
  (interactive "P")
  (ar-th-forward 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-backward-triplebackticked-atpt (&optional no-delimiters)
  "Moves backward over TRIPLEBACKTICKED.
Returns beginning position of TRIPLEBACKTICKED "
  (interactive "P")
  (ar-th-backward 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-transpose-triplebackticked-atpt (&optional no-delimiters)
  "Transposes TRIPLEBACKTICKED with TRIPLEBACKTICKED before point if any. "
  (interactive "*P")
  (ar-th-transpose 'triplebackticked no-delimiters))

;;;###autoload
(defun ar-sort-triplebackticked-atpt (&optional reverse)
  "Sorts triplebacktickeds in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'triplebackticked reverse))

;;;###autoload
(defun ar-check-triplebackticked-atpt (&optional arg)
  "Return t if a TRIPLEBACKTICKED at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-triplebackticked-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-triplebackticked-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-xslstylesheet-atpt (&optional no-delimiters)
  "Returns xslstylesheet at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-bounds-of-xslstylesheet-atpt (&optional no-delimiters)
  "Returns a list, borders of xslstylesheet if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-xslstylesheet-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position XSLSTYLESHEET.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-xslstylesheet-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of XSLSTYLESHEET.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-beginning-of-xslstylesheet-atpt (&optional no-delimiters)
  "Goto beginning of XSLSTYLESHEET.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-end-of-xslstylesheet-atpt (&optional no-delimiters)
  "Goto end of XSLSTYLESHEET at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-in-xslstylesheet-p-atpt (&optional no-delimiters)
  "Returns bounds of XSLSTYLESHEET if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-length-of-xslstylesheet-atpt (&optional no-delimiters)
  "Returns beginning of XSLSTYLESHEET at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-copy-xslstylesheet-atpt (&optional no-delimiters)
  "Returns a copy of XSLSTYLESHEET. "
  (interactive "P")
  (ar-th-copy 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-delete-xslstylesheet-atpt (&optional no-delimiters)
  "Deletes XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-delete 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-delete-xslstylesheet-in-region (beg end)
  "Deletes XSLSTYLESHEET at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'xslstylesheet beg end))

;;;###autoload
(defun ar-blok-xslstylesheet-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around xslstylesheet.
  Returns blok or nil if no XSLSTYLESHEET at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-backslashparen-xslstylesheet-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around xslstylesheet at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-doublebackslash-xslstylesheet-atpt (&optional no-delimiters)
  "Puts doubled backslashes around XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-doubleslash-xslstylesheet-atpt (&optional no-delimiters)
  "Puts doubled slashes around XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-xslstylesheet-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-doublebacktick-xslstylesheet-atpt (&optional no-delimiters)
  "Provides double backticks around XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-slashparen-xslstylesheet-atpt (&optional no-delimiters)
  "Provides slashed parentheses around XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-comment-xslstylesheet-atpt (&optional no-delimiters)
  "Comments XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-comment 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-commatize-xslstylesheet-atpt (&optional no-delimiters)
  "Put a comma after XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-commatize 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-quote-xslstylesheet-atpt (&optional no-delimiters)
  "Put a singlequote before XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-quote 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-mark-xslstylesheet-atpt (&optional no-delimiters)
  "Marks XSLSTYLESHEET at point if any. "
  (interactive "P")
  (ar-th-mark 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-hide-xslstylesheet-atpt (&optional no-delimiters)
  "Hides XSLSTYLESHEET at point. "
  (interactive "P")
  (ar-th-hide 'xslstylesheet nil nil no-delimiters))

;;;###autoload
(defun ar-show-xslstylesheet-atpt (&optional no-delimiters)
  "Shows hidden XSLSTYLESHEET at point. "
  (interactive "P")
  (ar-th-show 'xslstylesheet nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-xslstylesheet-atpt (&optional no-delimiters)
  "Alternatively hides or shows XSLSTYLESHEET at point. "
  (interactive "P")
  (ar-th-hide-show 'xslstylesheet nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-xslstylesheet-atpt-mode (&optional no-delimiters)
  "Toggles xslstylesheet-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-kill-xslstylesheet-atpt (&optional no-delimiters)
  "Kills XSLSTYLESHEET at point if any. "
  (interactive "*P")
  (ar-th-kill 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-separate-xslstylesheet-atpt (&optional no-delimiters)
  "Separates XSLSTYLESHEET at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-triplequotedq-xslstylesheet-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around xslstylesheet. "
  (interactive "*P")
  (ar-th-triplequotedq 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-triplequotesq-xslstylesheet-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around xslstylesheet. "
  (interactive "*P")
  (ar-th-triplequotesq 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-triplebacktick-xslstylesheet-atpt (&optional no-delimiters)
  "Triplebacktick xslstylesheet at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-trim-xslstylesheet-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'xslstylesheet no-delimiters t t))

;;;###autoload
(defun ar-left-trim-xslstylesheet-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'xslstylesheet no-delimiters t))

;;;###autoload
(defun ar-right-trim-xslstylesheet-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'xslstylesheet no-delimiters nil t))

;;;###autoload
(defun ar-underscore-xslstylesheet-atpt (&optional no-delimiters)
  "Put underscore char around XSLSTYLESHEET. "
  (interactive "*P")
  (ar-th-underscore 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-forward-xslstylesheet-atpt (&optional no-delimiters)
  "Moves forward over XSLSTYLESHEET at point if any, does nothing otherwise.
Returns end position of XSLSTYLESHEET "
  (interactive "P")
  (ar-th-forward 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-backward-xslstylesheet-atpt (&optional no-delimiters)
  "Moves backward over XSLSTYLESHEET.
Returns beginning position of XSLSTYLESHEET "
  (interactive "P")
  (ar-th-backward 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-transpose-xslstylesheet-atpt (&optional no-delimiters)
  "Transposes XSLSTYLESHEET with XSLSTYLESHEET before point if any. "
  (interactive "*P")
  (ar-th-transpose 'xslstylesheet no-delimiters))

;;;###autoload
(defun ar-sort-xslstylesheet-atpt (&optional reverse)
  "Sorts xslstylesheets in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'xslstylesheet reverse))

;;;###autoload
(defun ar-check-xslstylesheet-atpt (&optional arg)
  "Return t if a XSLSTYLESHEET at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-xslstylesheet-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-xslstylesheet-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;;;###autoload
(defun ar-xsltemplate-atpt (&optional no-delimiters)
  "Returns xsltemplate at point if any, nil otherwise.

Optional \\[universal-argument] returns objects without delimiters"
  (interactive "P")
  (ar-th 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-bounds-of-xsltemplate-atpt (&optional no-delimiters)
  "Returns a list, borders of xsltemplate if any, nil otherwise.

Optional \\[universal-argument] returns bounds without delimiters"
  (interactive "P")
  (ar-th-bounds 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-xsltemplate-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position XSLTEMPLATE.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-beg 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-xsltemplate-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of XSLTEMPLATE.

Optional \\[universal-argument] returns end position at delimiter "
  (interactive "P")
  (ar-th-end 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-beginning-of-xsltemplate-atpt (&optional no-delimiters)
  "Goto beginning of XSLTEMPLATE.

Optional \\[universal-argument] returns start position after delimiter "
  (interactive "P")
  (ar-th-gotobeg 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-end-of-xsltemplate-atpt (&optional no-delimiters)
  "Goto end of XSLTEMPLATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-in-xsltemplate-p-atpt (&optional no-delimiters)
  "Returns bounds of XSLTEMPLATE if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-length-of-xsltemplate-atpt (&optional no-delimiters)
  "Returns beginning of XSLTEMPLATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-copy-xsltemplate-atpt (&optional no-delimiters)
  "Returns a copy of XSLTEMPLATE. "
  (interactive "P")
  (ar-th-copy 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-delete-xsltemplate-atpt (&optional no-delimiters)
  "Deletes XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-delete 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-delete-xsltemplate-in-region (beg end)
  "Deletes XSLTEMPLATE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'xsltemplate beg end))

;;;###autoload
(defun ar-blok-xsltemplate-atpt (&optional no-delimiters)
  "Puts ‘blok-startstring-atpt’, ‘blok-endstring-atpt’ around xsltemplate.
  Returns blok or nil if no XSLTEMPLATE at cursor-position. "
  (interactive "*P")
  (ar-th-blok 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-backslashparen-xsltemplate-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around xsltemplate at point if any.
With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-backslashparen 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-doublebackslash-xsltemplate-atpt (&optional no-delimiters)
  "Puts doubled backslashes around XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslash 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-doubleslash-xsltemplate-atpt (&optional no-delimiters)
  "Puts doubled slashes around XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-doubleslash 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-doublebackslashparen-xsltemplate-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-doublebackslashparen 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-doublebacktick-xsltemplate-atpt (&optional no-delimiters)
  "Provides double backticks around XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-doublebacktick 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-slashparen-xsltemplate-atpt (&optional no-delimiters)
  "Provides slashed parentheses around XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-slashparen 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-comment-xsltemplate-atpt (&optional no-delimiters)
  "Comments XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-comment 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-commatize-xsltemplate-atpt (&optional no-delimiters)
  "Put a comma after XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-commatize 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-quote-xsltemplate-atpt (&optional no-delimiters)
  "Put a singlequote before XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-quote 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-mark-xsltemplate-atpt (&optional no-delimiters)
  "Marks XSLTEMPLATE at point if any. "
  (interactive "P")
  (ar-th-mark 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-hide-xsltemplate-atpt (&optional no-delimiters)
  "Hides XSLTEMPLATE at point. "
  (interactive "P")
  (ar-th-hide 'xsltemplate nil nil no-delimiters))

;;;###autoload
(defun ar-show-xsltemplate-atpt (&optional no-delimiters)
  "Shows hidden XSLTEMPLATE at point. "
  (interactive "P")
  (ar-th-show 'xsltemplate nil nil no-delimiters))

;;;###autoload
(defun ar-hide-show-xsltemplate-atpt (&optional no-delimiters)
  "Alternatively hides or shows XSLTEMPLATE at point. "
  (interactive "P")
  (ar-th-hide-show 'xsltemplate nil nil no-delimiters))

;;;###autoload
(defun ar-highlight-xsltemplate-atpt-mode (&optional no-delimiters)
  "Toggles xsltemplate-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-kill-xsltemplate-atpt (&optional no-delimiters)
  "Kills XSLTEMPLATE at point if any. "
  (interactive "*P")
  (ar-th-kill 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-separate-xsltemplate-atpt (&optional no-delimiters)
  "Separates XSLTEMPLATE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*P")
  (ar-th-separate 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-triplequotedq-xsltemplate-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around xsltemplate. "
  (interactive "*P")
  (ar-th-triplequotedq 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-triplequotesq-xsltemplate-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around xsltemplate. "
  (interactive "*P")
  (ar-th-triplequotesq 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-triplebacktick-xsltemplate-atpt (&optional no-delimiters)
  "Triplebacktick xsltemplate at point.

With optional \\[universal-argument] NO-DELIMITERS resp. to inner position of delimiter "
  (interactive "*P")
  (ar-th-triplebacktick 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-trim-xsltemplate-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'xsltemplate no-delimiters t t))

;;;###autoload
(defun ar-left-trim-xsltemplate-atpt (&optional no-delimiters)
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'xsltemplate no-delimiters t))

;;;###autoload
(defun ar-right-trim-xsltemplate-atpt (&optional no-delimiters)
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'xsltemplate no-delimiters nil t))

;;;###autoload
(defun ar-underscore-xsltemplate-atpt (&optional no-delimiters)
  "Put underscore char around XSLTEMPLATE. "
  (interactive "*P")
  (ar-th-underscore 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-forward-xsltemplate-atpt (&optional no-delimiters)
  "Moves forward over XSLTEMPLATE at point if any, does nothing otherwise.
Returns end position of XSLTEMPLATE "
  (interactive "P")
  (ar-th-forward 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-backward-xsltemplate-atpt (&optional no-delimiters)
  "Moves backward over XSLTEMPLATE.
Returns beginning position of XSLTEMPLATE "
  (interactive "P")
  (ar-th-backward 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-transpose-xsltemplate-atpt (&optional no-delimiters)
  "Transposes XSLTEMPLATE with XSLTEMPLATE before point if any. "
  (interactive "*P")
  (ar-th-transpose 'xsltemplate no-delimiters))

;;;###autoload
(defun ar-sort-xsltemplate-atpt (&optional reverse)
  "Sorts xsltemplates in at point, with \\[universal-argument] in reverse order.
  "
  (interactive "*P")
  (ar-th-sort 'xsltemplate reverse))

;;;###autoload
(defun ar-check-xsltemplate-atpt (&optional arg)
  "Return t if a XSLTEMPLATE at point exists, nil otherwise "
  (interactive "P")
  (let* ((beg (funcall (intern-soft (concat "ar-xsltemplate-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-xsltemplate-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when arg (message "%s" erg))
   erg))

;; ar-thingatpt-utils-delimiters-core: ar-atpt-markup-list end



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

;;;###autoload
(defun ar-go-in-statement-backward ()
  (interactive "p")
  (skip-chars-backward "=\" \t\r\n\f")
  (forward-char -1))

(defun ar-go-in-statement-backward-to-beg-of-symbol ()
  (ar-go-in-statement-backward)
  (ar-th-gotobeg 'symbol))

;;;###autoload
(defun ar-value-of-mlattribut-atpt (&optional arg)
  (interactive "p")
  "Return the value of attribute under cursor if any."
  (ar-end-of-mlattribut-atpt)
  (forward-char -1)
  (let ((value (ar-string-atpt arg)))
    (when arg (message "%s" value))
    value))

;;;###autoload
(defun ar-name-of-mlattribut-atpt (&optional arg) 
  (interactive "p")
  (ar-beginning-of-mlattribut-atpt)
  (let ((name (ar-alnum-atpt)))
    (when arg (message "%s" name))
    name))

;;;###autoload
(defun ar-mlattribut-or-name-atpt (&optional arg)
  "Returns mlattribut-atpt at point if any, nil otherwise.
  With \C-u ar-name-of-mlattribut-atpt is called "
  (interactive "P")
  (if (eq 4 (prefix-numeric-value arg))
    (ar-name-of-mlattribut-atpt)
  (ar-th 'mlattribut arg)))




(provide 'ar-thingatpt-markup)
;;; ar-thingatpt-markup.el ends here
