;;; thing-at-point-utils.el

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

(require 'thingatpt-utils-core)


;; ar-thing-at-point-utils-delimited-intern: ar-paired-delimited-passiv start

(defun ar-braced-atpt (&optional arg no-delimiters)
  "Returns braced at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'braced arg no-delimiters (interactive-p)))

(defun ar-bounds-of-braced-atpt (&optional no-delimiters)
  "Returns a list, borders of braced if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'braced no-delimiters (interactive-p)))

(defun ar-braced-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position braced at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'braced no-delimiters (interactive-p)))

(defun ar-braced-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of braced at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'braced no-delimiters (interactive-p)))

(defun ar-beginning-of-braced-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class braced at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'braced no-delimiters (interactive-p)))

(defun ar-end-of-braced-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class braced at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'braced no-delimiters (interactive-p)))

(defun ar-length-of-braced-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class braced at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'braced no-delimiters (interactive-p)))

(defun ar-copy-braced-atpt (&optional no-delimiters)
  "Returns a copy of braced at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'braced no-delimiters (interactive-p)))

(defun ar-delete-braced-atpt (&optional arg)
  "Deletes braced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'braced arg (interactive-p)))

(defun ar-delete-braced-in-region (beg end)
  "Deletes braced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'braced beg end (interactive-p)))

(defun ar-blok-braced-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around braced.

If region is active, do that for all elements \"braced\" in region.
  Returns blok or nil if no braced at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'braced no-delimiters (interactive-p)))

(defun ar-doublebackslash-braced-atpt (&optional no-delimiters)
  "Puts doubled backslashes around braced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'braced no-delimiters (interactive-p)))

(defun ar-doubleslash-braced-atpt (&optional no-delimiters)
  "Puts doubled slashes around braced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'braced no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-braced-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around braced at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'braced no-delimiters (interactive-p)))

;; (defun ar-slashparen-braced-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around braced at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'braced no-delimiters (interactive-p)))

(defun ar-comment-braced-atpt (&optional no-delimiters)
  "Comments braced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'braced no-delimiters (interactive-p)))

(defun ar-commatize-braced-atpt (&optional no-delimiters)
  "Put a comma after braced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'braced no-delimiters (interactive-p)))

(defun ar-mark-braced-atpt (&optional no-delimiters)
  "Marks braced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'braced))

(defun ar-hide-braced-atpt ()
  "Hides braced at point. "
  (interactive)
  (ar-th-hide 'braced))

(defun ar-show-braced-atpt ()
  "Shows hidden braced at point. "
  (interactive)
  (ar-th-show 'braced))

(defun ar-hide-show-braced-atpt ()
  "Alternatively hides or shows braced at point. "
  (interactive)
  (ar-th-hide-show 'braced))

(defun ar-highlight-braced-atpt-mode (&optional no-delimiters)
  "Toggles braced-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'braced no-delimiters (interactive-p)))

(defun ar-kill-braced-atpt (&optional no-delimiters)
  "Kills braced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'braced no-delimiters (interactive-p)))

(defun ar-kill-backward-braced-atpt (&optional no-delimiters)
  "Kills braced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'braced no-delimiters (interactive-p)))

(defun ar-separate-braced-atpt (&optional no-delimiters)
  "Separates braced at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'braced no-delimiters (interactive-p)))

(defun ar-forward-braced-atpt (&optional arg)
  "Moves forward over braced at point if any, does nothing otherwise.
Returns end position of braced "
  (interactive "p")
  (ar-th-forward 'braced arg (interactive-p)))

(defun ar-backward-braced-atpt (&optional arg)
  "Moves backward over braced before point if any, does nothing otherwise.
Returns beginning position of braced "
  (interactive "p")
  (ar-th-backward 'braced arg (interactive-p)))

(defun ar-transpose-braced-atpt (&optional arg)
  "Transposes braced with braced before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'braced arg (interactive-p)))

(defun ar-sort-braced-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts braceds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'braced reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-braced-atpt ()
  "Return t if a braced at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-braced-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-braced-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-bracketed-atpt (&optional arg no-delimiters)
  "Returns bracketed at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'bracketed arg no-delimiters (interactive-p)))

(defun ar-bounds-of-bracketed-atpt (&optional no-delimiters)
  "Returns a list, borders of bracketed if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'bracketed no-delimiters (interactive-p)))

(defun ar-bracketed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position bracketed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'bracketed no-delimiters (interactive-p)))

(defun ar-bracketed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of bracketed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'bracketed no-delimiters (interactive-p)))

(defun ar-beginning-of-bracketed-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class bracketed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'bracketed no-delimiters (interactive-p)))

(defun ar-end-of-bracketed-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class bracketed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'bracketed no-delimiters (interactive-p)))

(defun ar-length-of-bracketed-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class bracketed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'bracketed no-delimiters (interactive-p)))

(defun ar-copy-bracketed-atpt (&optional no-delimiters)
  "Returns a copy of bracketed at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'bracketed no-delimiters (interactive-p)))

(defun ar-delete-bracketed-atpt (&optional arg)
  "Deletes bracketed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'bracketed arg (interactive-p)))

(defun ar-delete-bracketed-in-region (beg end)
  "Deletes bracketed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'bracketed beg end (interactive-p)))

(defun ar-blok-bracketed-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around bracketed.

If region is active, do that for all elements \"bracketed\" in region.
  Returns blok or nil if no bracketed at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'bracketed no-delimiters (interactive-p)))

(defun ar-doublebackslash-bracketed-atpt (&optional no-delimiters)
  "Puts doubled backslashes around bracketed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'bracketed no-delimiters (interactive-p)))

(defun ar-doubleslash-bracketed-atpt (&optional no-delimiters)
  "Puts doubled slashes around bracketed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'bracketed no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-bracketed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around bracketed at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'bracketed no-delimiters (interactive-p)))

;; (defun ar-slashparen-bracketed-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around bracketed at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'bracketed no-delimiters (interactive-p)))

(defun ar-comment-bracketed-atpt (&optional no-delimiters)
  "Comments bracketed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'bracketed no-delimiters (interactive-p)))

(defun ar-commatize-bracketed-atpt (&optional no-delimiters)
  "Put a comma after bracketed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'bracketed no-delimiters (interactive-p)))

(defun ar-mark-bracketed-atpt (&optional no-delimiters)
  "Marks bracketed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'bracketed))

(defun ar-hide-bracketed-atpt ()
  "Hides bracketed at point. "
  (interactive)
  (ar-th-hide 'bracketed))

(defun ar-show-bracketed-atpt ()
  "Shows hidden bracketed at point. "
  (interactive)
  (ar-th-show 'bracketed))

(defun ar-hide-show-bracketed-atpt ()
  "Alternatively hides or shows bracketed at point. "
  (interactive)
  (ar-th-hide-show 'bracketed))

(defun ar-highlight-bracketed-atpt-mode (&optional no-delimiters)
  "Toggles bracketed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'bracketed no-delimiters (interactive-p)))

(defun ar-kill-bracketed-atpt (&optional no-delimiters)
  "Kills bracketed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'bracketed no-delimiters (interactive-p)))

(defun ar-kill-backward-bracketed-atpt (&optional no-delimiters)
  "Kills bracketed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'bracketed no-delimiters (interactive-p)))

(defun ar-separate-bracketed-atpt (&optional no-delimiters)
  "Separates bracketed at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'bracketed no-delimiters (interactive-p)))

(defun ar-forward-bracketed-atpt (&optional arg)
  "Moves forward over bracketed at point if any, does nothing otherwise.
Returns end position of bracketed "
  (interactive "p")
  (ar-th-forward 'bracketed arg (interactive-p)))

(defun ar-backward-bracketed-atpt (&optional arg)
  "Moves backward over bracketed before point if any, does nothing otherwise.
Returns beginning position of bracketed "
  (interactive "p")
  (ar-th-backward 'bracketed arg (interactive-p)))

(defun ar-transpose-bracketed-atpt (&optional arg)
  "Transposes bracketed with bracketed before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'bracketed arg (interactive-p)))

(defun ar-sort-bracketed-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts bracketeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'bracketed reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-bracketed-atpt ()
  "Return t if a bracketed at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-bracketed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-bracketed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-lesserangled-atpt (&optional arg no-delimiters)
  "Returns lesserangled at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'lesserangled arg no-delimiters (interactive-p)))

(defun ar-bounds-of-lesserangled-atpt (&optional no-delimiters)
  "Returns a list, borders of lesserangled if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'lesserangled no-delimiters (interactive-p)))

(defun ar-lesserangled-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position lesserangled at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'lesserangled no-delimiters (interactive-p)))

(defun ar-lesserangled-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of lesserangled at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'lesserangled no-delimiters (interactive-p)))

(defun ar-beginning-of-lesserangled-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class lesserangled at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'lesserangled no-delimiters (interactive-p)))

(defun ar-end-of-lesserangled-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class lesserangled at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'lesserangled no-delimiters (interactive-p)))

(defun ar-length-of-lesserangled-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class lesserangled at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'lesserangled no-delimiters (interactive-p)))

(defun ar-copy-lesserangled-atpt (&optional no-delimiters)
  "Returns a copy of lesserangled at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'lesserangled no-delimiters (interactive-p)))

(defun ar-delete-lesserangled-atpt (&optional arg)
  "Deletes lesserangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'lesserangled arg (interactive-p)))

(defun ar-delete-lesserangled-in-region (beg end)
  "Deletes lesserangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'lesserangled beg end (interactive-p)))

(defun ar-blok-lesserangled-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around lesserangled.

If region is active, do that for all elements \"lesserangled\" in region.
  Returns blok or nil if no lesserangled at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'lesserangled no-delimiters (interactive-p)))

(defun ar-doublebackslash-lesserangled-atpt (&optional no-delimiters)
  "Puts doubled backslashes around lesserangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'lesserangled no-delimiters (interactive-p)))

(defun ar-doubleslash-lesserangled-atpt (&optional no-delimiters)
  "Puts doubled slashes around lesserangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'lesserangled no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-lesserangled-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around lesserangled at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'lesserangled no-delimiters (interactive-p)))

;; (defun ar-slashparen-lesserangled-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around lesserangled at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'lesserangled no-delimiters (interactive-p)))

(defun ar-comment-lesserangled-atpt (&optional no-delimiters)
  "Comments lesserangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'lesserangled no-delimiters (interactive-p)))

(defun ar-commatize-lesserangled-atpt (&optional no-delimiters)
  "Put a comma after lesserangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'lesserangled no-delimiters (interactive-p)))

(defun ar-mark-lesserangled-atpt (&optional no-delimiters)
  "Marks lesserangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'lesserangled))

(defun ar-hide-lesserangled-atpt ()
  "Hides lesserangled at point. "
  (interactive)
  (ar-th-hide 'lesserangled))

(defun ar-show-lesserangled-atpt ()
  "Shows hidden lesserangled at point. "
  (interactive)
  (ar-th-show 'lesserangled))

(defun ar-hide-show-lesserangled-atpt ()
  "Alternatively hides or shows lesserangled at point. "
  (interactive)
  (ar-th-hide-show 'lesserangled))

(defun ar-highlight-lesserangled-atpt-mode (&optional no-delimiters)
  "Toggles lesserangled-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'lesserangled no-delimiters (interactive-p)))

(defun ar-kill-lesserangled-atpt (&optional no-delimiters)
  "Kills lesserangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'lesserangled no-delimiters (interactive-p)))

(defun ar-kill-backward-lesserangled-atpt (&optional no-delimiters)
  "Kills lesserangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'lesserangled no-delimiters (interactive-p)))

(defun ar-separate-lesserangled-atpt (&optional no-delimiters)
  "Separates lesserangled at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'lesserangled no-delimiters (interactive-p)))

(defun ar-forward-lesserangled-atpt (&optional arg)
  "Moves forward over lesserangled at point if any, does nothing otherwise.
Returns end position of lesserangled "
  (interactive "p")
  (ar-th-forward 'lesserangled arg (interactive-p)))

(defun ar-backward-lesserangled-atpt (&optional arg)
  "Moves backward over lesserangled before point if any, does nothing otherwise.
Returns beginning position of lesserangled "
  (interactive "p")
  (ar-th-backward 'lesserangled arg (interactive-p)))

(defun ar-transpose-lesserangled-atpt (&optional arg)
  "Transposes lesserangled with lesserangled before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'lesserangled arg (interactive-p)))

(defun ar-sort-lesserangled-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lesserangleds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'lesserangled reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-lesserangled-atpt ()
  "Return t if a lesserangled at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-lesserangled-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-lesserangled-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-greaterangled-atpt (&optional arg no-delimiters)
  "Returns greaterangled at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'greaterangled arg no-delimiters (interactive-p)))

(defun ar-bounds-of-greaterangled-atpt (&optional no-delimiters)
  "Returns a list, borders of greaterangled if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'greaterangled no-delimiters (interactive-p)))

(defun ar-greaterangled-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position greaterangled at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'greaterangled no-delimiters (interactive-p)))

(defun ar-greaterangled-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of greaterangled at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'greaterangled no-delimiters (interactive-p)))

(defun ar-beginning-of-greaterangled-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class greaterangled at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'greaterangled no-delimiters (interactive-p)))

(defun ar-end-of-greaterangled-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class greaterangled at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'greaterangled no-delimiters (interactive-p)))

(defun ar-length-of-greaterangled-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class greaterangled at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'greaterangled no-delimiters (interactive-p)))

(defun ar-copy-greaterangled-atpt (&optional no-delimiters)
  "Returns a copy of greaterangled at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'greaterangled no-delimiters (interactive-p)))

(defun ar-delete-greaterangled-atpt (&optional arg)
  "Deletes greaterangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'greaterangled arg (interactive-p)))

(defun ar-delete-greaterangled-in-region (beg end)
  "Deletes greaterangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'greaterangled beg end (interactive-p)))

(defun ar-blok-greaterangled-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around greaterangled.

If region is active, do that for all elements \"greaterangled\" in region.
  Returns blok or nil if no greaterangled at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'greaterangled no-delimiters (interactive-p)))

(defun ar-doublebackslash-greaterangled-atpt (&optional no-delimiters)
  "Puts doubled backslashes around greaterangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'greaterangled no-delimiters (interactive-p)))

(defun ar-doubleslash-greaterangled-atpt (&optional no-delimiters)
  "Puts doubled slashes around greaterangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'greaterangled no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-greaterangled-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around greaterangled at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'greaterangled no-delimiters (interactive-p)))

;; (defun ar-slashparen-greaterangled-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around greaterangled at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'greaterangled no-delimiters (interactive-p)))

(defun ar-comment-greaterangled-atpt (&optional no-delimiters)
  "Comments greaterangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'greaterangled no-delimiters (interactive-p)))

(defun ar-commatize-greaterangled-atpt (&optional no-delimiters)
  "Put a comma after greaterangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'greaterangled no-delimiters (interactive-p)))

(defun ar-mark-greaterangled-atpt (&optional no-delimiters)
  "Marks greaterangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'greaterangled))

(defun ar-hide-greaterangled-atpt ()
  "Hides greaterangled at point. "
  (interactive)
  (ar-th-hide 'greaterangled))

(defun ar-show-greaterangled-atpt ()
  "Shows hidden greaterangled at point. "
  (interactive)
  (ar-th-show 'greaterangled))

(defun ar-hide-show-greaterangled-atpt ()
  "Alternatively hides or shows greaterangled at point. "
  (interactive)
  (ar-th-hide-show 'greaterangled))

(defun ar-highlight-greaterangled-atpt-mode (&optional no-delimiters)
  "Toggles greaterangled-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'greaterangled no-delimiters (interactive-p)))

(defun ar-kill-greaterangled-atpt (&optional no-delimiters)
  "Kills greaterangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'greaterangled no-delimiters (interactive-p)))

(defun ar-kill-backward-greaterangled-atpt (&optional no-delimiters)
  "Kills greaterangled at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'greaterangled no-delimiters (interactive-p)))

(defun ar-separate-greaterangled-atpt (&optional no-delimiters)
  "Separates greaterangled at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'greaterangled no-delimiters (interactive-p)))

(defun ar-forward-greaterangled-atpt (&optional arg)
  "Moves forward over greaterangled at point if any, does nothing otherwise.
Returns end position of greaterangled "
  (interactive "p")
  (ar-th-forward 'greaterangled arg (interactive-p)))

(defun ar-backward-greaterangled-atpt (&optional arg)
  "Moves backward over greaterangled before point if any, does nothing otherwise.
Returns beginning position of greaterangled "
  (interactive "p")
  (ar-th-backward 'greaterangled arg (interactive-p)))

(defun ar-transpose-greaterangled-atpt (&optional arg)
  "Transposes greaterangled with greaterangled before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'greaterangled arg (interactive-p)))

(defun ar-sort-greaterangled-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts greaterangleds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'greaterangled reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-greaterangled-atpt ()
  "Return t if a greaterangled at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-greaterangled-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-greaterangled-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-leftrightsinglequoted-atpt (&optional arg no-delimiters)
  "Returns leftrightsinglequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'leftrightsinglequoted arg no-delimiters (interactive-p)))

(defun ar-bounds-of-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Returns a list, borders of leftrightsinglequoted if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-leftrightsinglequoted-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position leftrightsinglequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-leftrightsinglequoted-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of leftrightsinglequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-beginning-of-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class leftrightsinglequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-end-of-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class leftrightsinglequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-length-of-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class leftrightsinglequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-copy-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Returns a copy of leftrightsinglequoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-delete-leftrightsinglequoted-atpt (&optional arg)
  "Deletes leftrightsinglequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'leftrightsinglequoted arg (interactive-p)))

(defun ar-delete-leftrightsinglequoted-in-region (beg end)
  "Deletes leftrightsinglequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'leftrightsinglequoted beg end (interactive-p)))

(defun ar-blok-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around leftrightsinglequoted.

If region is active, do that for all elements \"leftrightsinglequoted\" in region.
  Returns blok or nil if no leftrightsinglequoted at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-doublebackslash-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Puts doubled backslashes around leftrightsinglequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-doubleslash-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Puts doubled slashes around leftrightsinglequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around leftrightsinglequoted at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'leftrightsinglequoted no-delimiters (interactive-p)))

;; (defun ar-slashparen-leftrightsinglequoted-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around leftrightsinglequoted at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-comment-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Comments leftrightsinglequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-commatize-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Put a comma after leftrightsinglequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-mark-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Marks leftrightsinglequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'leftrightsinglequoted))

(defun ar-hide-leftrightsinglequoted-atpt ()
  "Hides leftrightsinglequoted at point. "
  (interactive)
  (ar-th-hide 'leftrightsinglequoted))

(defun ar-show-leftrightsinglequoted-atpt ()
  "Shows hidden leftrightsinglequoted at point. "
  (interactive)
  (ar-th-show 'leftrightsinglequoted))

(defun ar-hide-show-leftrightsinglequoted-atpt ()
  "Alternatively hides or shows leftrightsinglequoted at point. "
  (interactive)
  (ar-th-hide-show 'leftrightsinglequoted))

(defun ar-highlight-leftrightsinglequoted-atpt-mode (&optional no-delimiters)
  "Toggles leftrightsinglequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-kill-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Kills leftrightsinglequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-kill-backward-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Kills leftrightsinglequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-separate-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Separates leftrightsinglequoted at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-forward-leftrightsinglequoted-atpt (&optional arg)
  "Moves forward over leftrightsinglequoted at point if any, does nothing otherwise.
Returns end position of leftrightsinglequoted "
  (interactive "p")
  (ar-th-forward 'leftrightsinglequoted arg (interactive-p)))

(defun ar-backward-leftrightsinglequoted-atpt (&optional arg)
  "Moves backward over leftrightsinglequoted before point if any, does nothing otherwise.
Returns beginning position of leftrightsinglequoted "
  (interactive "p")
  (ar-th-backward 'leftrightsinglequoted arg (interactive-p)))

(defun ar-transpose-leftrightsinglequoted-atpt (&optional arg)
  "Transposes leftrightsinglequoted with leftrightsinglequoted before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'leftrightsinglequoted arg (interactive-p)))

(defun ar-sort-leftrightsinglequoted-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts leftrightsinglequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'leftrightsinglequoted reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-leftrightsinglequoted-atpt ()
  "Return t if a leftrightsinglequoted at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-leftrightsinglequoted-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-leftrightsinglequoted-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-parentized-atpt (&optional arg no-delimiters)
  "Returns parentized at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'parentized arg no-delimiters (interactive-p)))

(defun ar-bounds-of-parentized-atpt (&optional no-delimiters)
  "Returns a list, borders of parentized if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'parentized no-delimiters (interactive-p)))

(defun ar-parentized-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position parentized at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'parentized no-delimiters (interactive-p)))

(defun ar-parentized-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of parentized at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'parentized no-delimiters (interactive-p)))

(defun ar-beginning-of-parentized-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class parentized at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'parentized no-delimiters (interactive-p)))

(defun ar-end-of-parentized-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class parentized at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'parentized no-delimiters (interactive-p)))

(defun ar-length-of-parentized-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class parentized at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'parentized no-delimiters (interactive-p)))

(defun ar-copy-parentized-atpt (&optional no-delimiters)
  "Returns a copy of parentized at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'parentized no-delimiters (interactive-p)))

(defun ar-delete-parentized-atpt (&optional arg)
  "Deletes parentized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'parentized arg (interactive-p)))

(defun ar-delete-parentized-in-region (beg end)
  "Deletes parentized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'parentized beg end (interactive-p)))

(defun ar-blok-parentized-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around parentized.

If region is active, do that for all elements \"parentized\" in region.
  Returns blok or nil if no parentized at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'parentized no-delimiters (interactive-p)))

(defun ar-doublebackslash-parentized-atpt (&optional no-delimiters)
  "Puts doubled backslashes around parentized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'parentized no-delimiters (interactive-p)))

(defun ar-doubleslash-parentized-atpt (&optional no-delimiters)
  "Puts doubled slashes around parentized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'parentized no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-parentized-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around parentized at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'parentized no-delimiters (interactive-p)))

;; (defun ar-slashparen-parentized-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around parentized at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'parentized no-delimiters (interactive-p)))

(defun ar-comment-parentized-atpt (&optional no-delimiters)
  "Comments parentized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'parentized no-delimiters (interactive-p)))

(defun ar-commatize-parentized-atpt (&optional no-delimiters)
  "Put a comma after parentized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'parentized no-delimiters (interactive-p)))

(defun ar-mark-parentized-atpt (&optional no-delimiters)
  "Marks parentized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'parentized))

(defun ar-hide-parentized-atpt ()
  "Hides parentized at point. "
  (interactive)
  (ar-th-hide 'parentized))

(defun ar-show-parentized-atpt ()
  "Shows hidden parentized at point. "
  (interactive)
  (ar-th-show 'parentized))

(defun ar-hide-show-parentized-atpt ()
  "Alternatively hides or shows parentized at point. "
  (interactive)
  (ar-th-hide-show 'parentized))

(defun ar-highlight-parentized-atpt-mode (&optional no-delimiters)
  "Toggles parentized-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'parentized no-delimiters (interactive-p)))

(defun ar-kill-parentized-atpt (&optional no-delimiters)
  "Kills parentized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'parentized no-delimiters (interactive-p)))

(defun ar-kill-backward-parentized-atpt (&optional no-delimiters)
  "Kills parentized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'parentized no-delimiters (interactive-p)))

(defun ar-separate-parentized-atpt (&optional no-delimiters)
  "Separates parentized at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'parentized no-delimiters (interactive-p)))

(defun ar-forward-parentized-atpt (&optional arg)
  "Moves forward over parentized at point if any, does nothing otherwise.
Returns end position of parentized "
  (interactive "p")
  (ar-th-forward 'parentized arg (interactive-p)))

(defun ar-backward-parentized-atpt (&optional arg)
  "Moves backward over parentized before point if any, does nothing otherwise.
Returns beginning position of parentized "
  (interactive "p")
  (ar-th-backward 'parentized arg (interactive-p)))

(defun ar-transpose-parentized-atpt (&optional arg)
  "Transposes parentized with parentized before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'parentized arg (interactive-p)))

(defun ar-sort-parentized-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts parentizeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'parentized reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-parentized-atpt ()
  "Return t if a parentized at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-parentized-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-parentized-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimited-intern: ar-paired-delimited-passiv end

;; ar-thing-at-point-utils-rest-unpaired-delimit ar-atpt-data-forms-aktiv-raw  (list (list 'list))): start
(defun ar-beginendquote-list-atpt (&optional arg)
  "Beginendquote LIST at point."
  (interactive "*p")
  (ar-th-beginendquote 'list arg arg))

(defun ar-blok-list-atpt (&optional arg)
  "Blok LIST at point."
  (interactive "*p")
  (ar-th-blok 'list arg arg))

(defun ar-doublebackslash-list-atpt (&optional arg)
  "Doublebackslash LIST at point."
  (interactive "*p")
  (ar-th-doublebackslash 'list arg arg))

(defun ar-doublebackslashparen-list-atpt (&optional arg)
  "Doublebackslashparen LIST at point."
  (interactive "*p")
  (ar-th-doublebackslashparen 'list arg arg))

(defun ar-doubleslash-list-atpt (&optional arg)
  "Doubleslash LIST at point."
  (interactive "*p")
  (ar-th-doubleslash 'list arg arg))

(defun ar-backslashparen-list-atpt (&optional arg)
  "Backslashparen LIST at point."
  (interactive "*p")
  (ar-th-backslashparen 'list arg arg))

(defun ar-slashparen-list-atpt (&optional arg)
  "Slashparen LIST at point."
  (interactive "*p")
  (ar-th-slashparen 'list arg arg))

;; ar-thing-at-point-utils-rest-unpaired-delimit ar-atpt-data-forms-aktiv-raw (list (list 'list))): end
 ;;ar-thing-at-point-utils-list-delimit ar-unpaired-delimit-aktiv-zahlenform (list 'list): start
(defun ar-backslash-list-atpt (&optional arg)
  "Backslash LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 92 92 arg arg))

(defun ar-backtick-list-atpt (&optional arg)
  "Backtick LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 96 96 arg arg))

(defun ar-colon-list-atpt (&optional arg)
  "Colon LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 58 58 arg arg))

(defun ar-cross-list-atpt (&optional arg)
  "Cross LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 43 43 arg arg))

(defun ar-dollar-list-atpt (&optional arg)
  "Dollar LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 36 36 arg arg))

(defun ar-doublequote-list-atpt (&optional arg)
  "Doublequote LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 34 34 arg arg))

(defun ar-equalize-list-atpt (&optional arg)
  "Equalize LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 61 61 arg arg))

(defun ar-hash-list-atpt (&optional arg)
  "Hash LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 35 35 arg arg))

(defun ar-hyphen-list-atpt (&optional arg)
  "Hyphen LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 45 45 arg arg))

(defun ar-singlequote-list-atpt (&optional arg)
  "Singlequote LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 39 39 arg arg))

(defun ar-slash-list-atpt (&optional arg)
  "Slash LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 47 47 arg arg))

(defun ar-star-list-atpt (&optional arg)
  "Star LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 42 42 arg arg))

(defun ar-tild-list-atpt (&optional arg)
  "Tild LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 126 126 arg arg))

(defun ar-underscore-list-atpt (&optional arg)
  "Underscore LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 95 95 arg arg))

(defun ar-whitespace-list-atpt (&optional arg)
  "Whitespace LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list 32 32 arg arg))

 ;;ar-thing-at-point-utils-list-delimit ar-unpaired-delimit-aktiv-zahlenform (list 'list): end

;; ar-thing-at-point-utils-rest-unpaired-delimit ar-unpaired-delimlist-aktiv  ar-atpt-rest-list: start
(defun ar-backslash-greateranglednested-atpt (&optional arg)
  "Backslash GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-backslash 'greateranglednested arg arg))

(defun ar-backslash-lesseranglednested-atpt (&optional arg)
  "Backslash LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-backslash 'lesseranglednested arg arg))

(defun ar-backslash-buffer-atpt (&optional arg)
  "Backslash BUFFER at point."
  (interactive "*p")
  (ar-th-backslash 'buffer arg arg))

(defun ar-backslash-comment-atpt (&optional arg)
  "Backslash COMMENT at point."
  (interactive "*p")
  (ar-th-backslash 'comment arg arg))

(defun ar-backslash-csv-atpt (&optional arg)
  "Backslash CSV at point."
  (interactive "*p")
  (ar-th-backslash 'csv arg arg))

(defun ar-backslash-date-atpt (&optional arg)
  "Backslash DATE at point."
  (interactive "*p")
  (ar-th-backslash 'date arg arg))

(defun ar-backslash-delimited-atpt (&optional arg)
  "Backslash DELIMITED at point."
  (interactive "*p")
  (ar-th-backslash 'delimited arg arg))

(defun ar-backslash-email-atpt (&optional arg)
  "Backslash EMAIL at point."
  (interactive "*p")
  (ar-th-backslash 'email arg arg))

(defun ar-backslash-filename-atpt (&optional arg)
  "Backslash FILENAME at point."
  (interactive "*p")
  (ar-th-backslash 'filename arg arg))

(defun ar-backslash-filenamenondirectory-atpt (&optional arg)
  "Backslash FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-backslash 'filenamenondirectory arg arg))

(defun ar-backslash-float-atpt (&optional arg)
  "Backslash FLOAT at point."
  (interactive "*p")
  (ar-th-backslash 'float arg arg))

(defun ar-backslash-function-atpt (&optional arg)
  "Backslash FUNCTION at point."
  (interactive "*p")
  (ar-th-backslash 'function arg arg))

(defun ar-backslash-ip-atpt (&optional arg)
  "Backslash IP at point."
  (interactive "*p")
  (ar-th-backslash 'ip arg arg))

(defun ar-backslash-isbn-atpt (&optional arg)
  "Backslash ISBN at point."
  (interactive "*p")
  (ar-th-backslash 'isbn arg arg))

(defun ar-backslash-line-atpt (&optional arg)
  "Backslash LINE at point."
  (interactive "*p")
  (ar-th-backslash 'line arg arg))

(defun ar-backslash-list-atpt (&optional arg)
  "Backslash LIST at point."
  (interactive "*p")
  (ar-th-backslash 'list arg arg))

(defun ar-backslash-name-atpt (&optional arg)
  "Backslash NAME at point."
  (interactive "*p")
  (ar-th-backslash 'name arg arg))

(defun ar-backslash-number-atpt (&optional arg)
  "Backslash NUMBER at point."
  (interactive "*p")
  (ar-th-backslash 'number arg arg))

(defun ar-backslash-page-atpt (&optional arg)
  "Backslash PAGE at point."
  (interactive "*p")
  (ar-th-backslash 'page arg arg))

(defun ar-backslash-paragraph-atpt (&optional arg)
  "Backslash PARAGRAPH at point."
  (interactive "*p")
  (ar-th-backslash 'paragraph arg arg))

(defun ar-backslash-phone-atpt (&optional arg)
  "Backslash PHONE at point."
  (interactive "*p")
  (ar-th-backslash 'phone arg arg))

(defun ar-backslash-region-atpt (&optional arg)
  "Backslash REGION at point."
  (interactive "*p")
  (ar-th-backslash 'region arg arg))

(defun ar-backslash-sentence-atpt (&optional arg)
  "Backslash SENTENCE at point."
  (interactive "*p")
  (ar-th-backslash 'sentence arg arg))

(defun ar-backslash-sexp-atpt (&optional arg)
  "Backslash SEXP at point."
  (interactive "*p")
  (ar-th-backslash 'sexp arg arg))

(defun ar-backslash-string-atpt (&optional arg)
  "Backslash STRING at point."
  (interactive "*p")
  (ar-th-backslash 'string arg arg))

(defun ar-backslash-shstruct-atpt (&optional arg)
  "Backslash SHSTRUCT at point."
  (interactive "*p")
  (ar-th-backslash 'shstruct arg arg))

(defun ar-backslash-symbol-atpt (&optional arg)
  "Backslash SYMBOL at point."
  (interactive "*p")
  (ar-th-backslash 'symbol arg arg))

(defun ar-backslash-url-atpt (&optional arg)
  "Backslash URL at point."
  (interactive "*p")
  (ar-th-backslash 'url arg arg))

(defun ar-backslash-word-atpt (&optional arg)
  "Backslash WORD at point."
  (interactive "*p")
  (ar-th-backslash 'word arg arg))

(defun ar-backslash-wordalphaonly-atpt (&optional arg)
  "Backslash WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-backslash 'wordalphaonly arg arg))

(defun ar-backtick-greateranglednested-atpt (&optional arg)
  "Backtick GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-backtick 'greateranglednested arg arg))

(defun ar-backtick-lesseranglednested-atpt (&optional arg)
  "Backtick LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-backtick 'lesseranglednested arg arg))

(defun ar-backtick-buffer-atpt (&optional arg)
  "Backtick BUFFER at point."
  (interactive "*p")
  (ar-th-backtick 'buffer arg arg))

(defun ar-backtick-comment-atpt (&optional arg)
  "Backtick COMMENT at point."
  (interactive "*p")
  (ar-th-backtick 'comment arg arg))

(defun ar-backtick-csv-atpt (&optional arg)
  "Backtick CSV at point."
  (interactive "*p")
  (ar-th-backtick 'csv arg arg))

(defun ar-backtick-date-atpt (&optional arg)
  "Backtick DATE at point."
  (interactive "*p")
  (ar-th-backtick 'date arg arg))

(defun ar-backtick-delimited-atpt (&optional arg)
  "Backtick DELIMITED at point."
  (interactive "*p")
  (ar-th-backtick 'delimited arg arg))

(defun ar-backtick-email-atpt (&optional arg)
  "Backtick EMAIL at point."
  (interactive "*p")
  (ar-th-backtick 'email arg arg))

(defun ar-backtick-filename-atpt (&optional arg)
  "Backtick FILENAME at point."
  (interactive "*p")
  (ar-th-backtick 'filename arg arg))

(defun ar-backtick-filenamenondirectory-atpt (&optional arg)
  "Backtick FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-backtick 'filenamenondirectory arg arg))

(defun ar-backtick-float-atpt (&optional arg)
  "Backtick FLOAT at point."
  (interactive "*p")
  (ar-th-backtick 'float arg arg))

(defun ar-backtick-function-atpt (&optional arg)
  "Backtick FUNCTION at point."
  (interactive "*p")
  (ar-th-backtick 'function arg arg))

(defun ar-backtick-ip-atpt (&optional arg)
  "Backtick IP at point."
  (interactive "*p")
  (ar-th-backtick 'ip arg arg))

(defun ar-backtick-isbn-atpt (&optional arg)
  "Backtick ISBN at point."
  (interactive "*p")
  (ar-th-backtick 'isbn arg arg))

(defun ar-backtick-line-atpt (&optional arg)
  "Backtick LINE at point."
  (interactive "*p")
  (ar-th-backtick 'line arg arg))

(defun ar-backtick-list-atpt (&optional arg)
  "Backtick LIST at point."
  (interactive "*p")
  (ar-th-backtick 'list arg arg))

(defun ar-backtick-name-atpt (&optional arg)
  "Backtick NAME at point."
  (interactive "*p")
  (ar-th-backtick 'name arg arg))

(defun ar-backtick-number-atpt (&optional arg)
  "Backtick NUMBER at point."
  (interactive "*p")
  (ar-th-backtick 'number arg arg))

(defun ar-backtick-page-atpt (&optional arg)
  "Backtick PAGE at point."
  (interactive "*p")
  (ar-th-backtick 'page arg arg))

(defun ar-backtick-paragraph-atpt (&optional arg)
  "Backtick PARAGRAPH at point."
  (interactive "*p")
  (ar-th-backtick 'paragraph arg arg))

(defun ar-backtick-phone-atpt (&optional arg)
  "Backtick PHONE at point."
  (interactive "*p")
  (ar-th-backtick 'phone arg arg))

(defun ar-backtick-region-atpt (&optional arg)
  "Backtick REGION at point."
  (interactive "*p")
  (ar-th-backtick 'region arg arg))

(defun ar-backtick-sentence-atpt (&optional arg)
  "Backtick SENTENCE at point."
  (interactive "*p")
  (ar-th-backtick 'sentence arg arg))

(defun ar-backtick-sexp-atpt (&optional arg)
  "Backtick SEXP at point."
  (interactive "*p")
  (ar-th-backtick 'sexp arg arg))

(defun ar-backtick-string-atpt (&optional arg)
  "Backtick STRING at point."
  (interactive "*p")
  (ar-th-backtick 'string arg arg))

(defun ar-backtick-shstruct-atpt (&optional arg)
  "Backtick SHSTRUCT at point."
  (interactive "*p")
  (ar-th-backtick 'shstruct arg arg))

(defun ar-backtick-symbol-atpt (&optional arg)
  "Backtick SYMBOL at point."
  (interactive "*p")
  (ar-th-backtick 'symbol arg arg))

(defun ar-backtick-url-atpt (&optional arg)
  "Backtick URL at point."
  (interactive "*p")
  (ar-th-backtick 'url arg arg))

(defun ar-backtick-word-atpt (&optional arg)
  "Backtick WORD at point."
  (interactive "*p")
  (ar-th-backtick 'word arg arg))

(defun ar-backtick-wordalphaonly-atpt (&optional arg)
  "Backtick WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-backtick 'wordalphaonly arg arg))

(defun ar-colon-greateranglednested-atpt (&optional arg)
  "Colon GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-colon 'greateranglednested arg arg))

(defun ar-colon-lesseranglednested-atpt (&optional arg)
  "Colon LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-colon 'lesseranglednested arg arg))

(defun ar-colon-buffer-atpt (&optional arg)
  "Colon BUFFER at point."
  (interactive "*p")
  (ar-th-colon 'buffer arg arg))

(defun ar-colon-comment-atpt (&optional arg)
  "Colon COMMENT at point."
  (interactive "*p")
  (ar-th-colon 'comment arg arg))

(defun ar-colon-csv-atpt (&optional arg)
  "Colon CSV at point."
  (interactive "*p")
  (ar-th-colon 'csv arg arg))

(defun ar-colon-date-atpt (&optional arg)
  "Colon DATE at point."
  (interactive "*p")
  (ar-th-colon 'date arg arg))

(defun ar-colon-delimited-atpt (&optional arg)
  "Colon DELIMITED at point."
  (interactive "*p")
  (ar-th-colon 'delimited arg arg))

(defun ar-colon-email-atpt (&optional arg)
  "Colon EMAIL at point."
  (interactive "*p")
  (ar-th-colon 'email arg arg))

(defun ar-colon-filename-atpt (&optional arg)
  "Colon FILENAME at point."
  (interactive "*p")
  (ar-th-colon 'filename arg arg))

(defun ar-colon-filenamenondirectory-atpt (&optional arg)
  "Colon FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-colon 'filenamenondirectory arg arg))

(defun ar-colon-float-atpt (&optional arg)
  "Colon FLOAT at point."
  (interactive "*p")
  (ar-th-colon 'float arg arg))

(defun ar-colon-function-atpt (&optional arg)
  "Colon FUNCTION at point."
  (interactive "*p")
  (ar-th-colon 'function arg arg))

(defun ar-colon-ip-atpt (&optional arg)
  "Colon IP at point."
  (interactive "*p")
  (ar-th-colon 'ip arg arg))

(defun ar-colon-isbn-atpt (&optional arg)
  "Colon ISBN at point."
  (interactive "*p")
  (ar-th-colon 'isbn arg arg))

(defun ar-colon-line-atpt (&optional arg)
  "Colon LINE at point."
  (interactive "*p")
  (ar-th-colon 'line arg arg))

(defun ar-colon-list-atpt (&optional arg)
  "Colon LIST at point."
  (interactive "*p")
  (ar-th-colon 'list arg arg))

(defun ar-colon-name-atpt (&optional arg)
  "Colon NAME at point."
  (interactive "*p")
  (ar-th-colon 'name arg arg))

(defun ar-colon-number-atpt (&optional arg)
  "Colon NUMBER at point."
  (interactive "*p")
  (ar-th-colon 'number arg arg))

(defun ar-colon-page-atpt (&optional arg)
  "Colon PAGE at point."
  (interactive "*p")
  (ar-th-colon 'page arg arg))

(defun ar-colon-paragraph-atpt (&optional arg)
  "Colon PARAGRAPH at point."
  (interactive "*p")
  (ar-th-colon 'paragraph arg arg))

(defun ar-colon-phone-atpt (&optional arg)
  "Colon PHONE at point."
  (interactive "*p")
  (ar-th-colon 'phone arg arg))

(defun ar-colon-region-atpt (&optional arg)
  "Colon REGION at point."
  (interactive "*p")
  (ar-th-colon 'region arg arg))

(defun ar-colon-sentence-atpt (&optional arg)
  "Colon SENTENCE at point."
  (interactive "*p")
  (ar-th-colon 'sentence arg arg))

(defun ar-colon-sexp-atpt (&optional arg)
  "Colon SEXP at point."
  (interactive "*p")
  (ar-th-colon 'sexp arg arg))

(defun ar-colon-string-atpt (&optional arg)
  "Colon STRING at point."
  (interactive "*p")
  (ar-th-colon 'string arg arg))

(defun ar-colon-shstruct-atpt (&optional arg)
  "Colon SHSTRUCT at point."
  (interactive "*p")
  (ar-th-colon 'shstruct arg arg))

(defun ar-colon-symbol-atpt (&optional arg)
  "Colon SYMBOL at point."
  (interactive "*p")
  (ar-th-colon 'symbol arg arg))

(defun ar-colon-url-atpt (&optional arg)
  "Colon URL at point."
  (interactive "*p")
  (ar-th-colon 'url arg arg))

(defun ar-colon-word-atpt (&optional arg)
  "Colon WORD at point."
  (interactive "*p")
  (ar-th-colon 'word arg arg))

(defun ar-colon-wordalphaonly-atpt (&optional arg)
  "Colon WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-colon 'wordalphaonly arg arg))

(defun ar-cross-greateranglednested-atpt (&optional arg)
  "Cross GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-cross 'greateranglednested arg arg))

(defun ar-cross-lesseranglednested-atpt (&optional arg)
  "Cross LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-cross 'lesseranglednested arg arg))

(defun ar-cross-buffer-atpt (&optional arg)
  "Cross BUFFER at point."
  (interactive "*p")
  (ar-th-cross 'buffer arg arg))

(defun ar-cross-comment-atpt (&optional arg)
  "Cross COMMENT at point."
  (interactive "*p")
  (ar-th-cross 'comment arg arg))

(defun ar-cross-csv-atpt (&optional arg)
  "Cross CSV at point."
  (interactive "*p")
  (ar-th-cross 'csv arg arg))

(defun ar-cross-date-atpt (&optional arg)
  "Cross DATE at point."
  (interactive "*p")
  (ar-th-cross 'date arg arg))

(defun ar-cross-delimited-atpt (&optional arg)
  "Cross DELIMITED at point."
  (interactive "*p")
  (ar-th-cross 'delimited arg arg))

(defun ar-cross-email-atpt (&optional arg)
  "Cross EMAIL at point."
  (interactive "*p")
  (ar-th-cross 'email arg arg))

(defun ar-cross-filename-atpt (&optional arg)
  "Cross FILENAME at point."
  (interactive "*p")
  (ar-th-cross 'filename arg arg))

(defun ar-cross-filenamenondirectory-atpt (&optional arg)
  "Cross FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-cross 'filenamenondirectory arg arg))

(defun ar-cross-float-atpt (&optional arg)
  "Cross FLOAT at point."
  (interactive "*p")
  (ar-th-cross 'float arg arg))

(defun ar-cross-function-atpt (&optional arg)
  "Cross FUNCTION at point."
  (interactive "*p")
  (ar-th-cross 'function arg arg))

(defun ar-cross-ip-atpt (&optional arg)
  "Cross IP at point."
  (interactive "*p")
  (ar-th-cross 'ip arg arg))

(defun ar-cross-isbn-atpt (&optional arg)
  "Cross ISBN at point."
  (interactive "*p")
  (ar-th-cross 'isbn arg arg))

(defun ar-cross-line-atpt (&optional arg)
  "Cross LINE at point."
  (interactive "*p")
  (ar-th-cross 'line arg arg))

(defun ar-cross-list-atpt (&optional arg)
  "Cross LIST at point."
  (interactive "*p")
  (ar-th-cross 'list arg arg))

(defun ar-cross-name-atpt (&optional arg)
  "Cross NAME at point."
  (interactive "*p")
  (ar-th-cross 'name arg arg))

(defun ar-cross-number-atpt (&optional arg)
  "Cross NUMBER at point."
  (interactive "*p")
  (ar-th-cross 'number arg arg))

(defun ar-cross-page-atpt (&optional arg)
  "Cross PAGE at point."
  (interactive "*p")
  (ar-th-cross 'page arg arg))

(defun ar-cross-paragraph-atpt (&optional arg)
  "Cross PARAGRAPH at point."
  (interactive "*p")
  (ar-th-cross 'paragraph arg arg))

(defun ar-cross-phone-atpt (&optional arg)
  "Cross PHONE at point."
  (interactive "*p")
  (ar-th-cross 'phone arg arg))

(defun ar-cross-region-atpt (&optional arg)
  "Cross REGION at point."
  (interactive "*p")
  (ar-th-cross 'region arg arg))

(defun ar-cross-sentence-atpt (&optional arg)
  "Cross SENTENCE at point."
  (interactive "*p")
  (ar-th-cross 'sentence arg arg))

(defun ar-cross-sexp-atpt (&optional arg)
  "Cross SEXP at point."
  (interactive "*p")
  (ar-th-cross 'sexp arg arg))

(defun ar-cross-string-atpt (&optional arg)
  "Cross STRING at point."
  (interactive "*p")
  (ar-th-cross 'string arg arg))

(defun ar-cross-shstruct-atpt (&optional arg)
  "Cross SHSTRUCT at point."
  (interactive "*p")
  (ar-th-cross 'shstruct arg arg))

(defun ar-cross-symbol-atpt (&optional arg)
  "Cross SYMBOL at point."
  (interactive "*p")
  (ar-th-cross 'symbol arg arg))

(defun ar-cross-url-atpt (&optional arg)
  "Cross URL at point."
  (interactive "*p")
  (ar-th-cross 'url arg arg))

(defun ar-cross-word-atpt (&optional arg)
  "Cross WORD at point."
  (interactive "*p")
  (ar-th-cross 'word arg arg))

(defun ar-cross-wordalphaonly-atpt (&optional arg)
  "Cross WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-cross 'wordalphaonly arg arg))

(defun ar-dollar-greateranglednested-atpt (&optional arg)
  "Dollar GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-dollar 'greateranglednested arg arg))

(defun ar-dollar-lesseranglednested-atpt (&optional arg)
  "Dollar LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-dollar 'lesseranglednested arg arg))

(defun ar-dollar-buffer-atpt (&optional arg)
  "Dollar BUFFER at point."
  (interactive "*p")
  (ar-th-dollar 'buffer arg arg))

(defun ar-dollar-comment-atpt (&optional arg)
  "Dollar COMMENT at point."
  (interactive "*p")
  (ar-th-dollar 'comment arg arg))

(defun ar-dollar-csv-atpt (&optional arg)
  "Dollar CSV at point."
  (interactive "*p")
  (ar-th-dollar 'csv arg arg))

(defun ar-dollar-date-atpt (&optional arg)
  "Dollar DATE at point."
  (interactive "*p")
  (ar-th-dollar 'date arg arg))

(defun ar-dollar-delimited-atpt (&optional arg)
  "Dollar DELIMITED at point."
  (interactive "*p")
  (ar-th-dollar 'delimited arg arg))

(defun ar-dollar-email-atpt (&optional arg)
  "Dollar EMAIL at point."
  (interactive "*p")
  (ar-th-dollar 'email arg arg))

(defun ar-dollar-filename-atpt (&optional arg)
  "Dollar FILENAME at point."
  (interactive "*p")
  (ar-th-dollar 'filename arg arg))

(defun ar-dollar-filenamenondirectory-atpt (&optional arg)
  "Dollar FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-dollar 'filenamenondirectory arg arg))

(defun ar-dollar-float-atpt (&optional arg)
  "Dollar FLOAT at point."
  (interactive "*p")
  (ar-th-dollar 'float arg arg))

(defun ar-dollar-function-atpt (&optional arg)
  "Dollar FUNCTION at point."
  (interactive "*p")
  (ar-th-dollar 'function arg arg))

(defun ar-dollar-ip-atpt (&optional arg)
  "Dollar IP at point."
  (interactive "*p")
  (ar-th-dollar 'ip arg arg))

(defun ar-dollar-isbn-atpt (&optional arg)
  "Dollar ISBN at point."
  (interactive "*p")
  (ar-th-dollar 'isbn arg arg))

(defun ar-dollar-line-atpt (&optional arg)
  "Dollar LINE at point."
  (interactive "*p")
  (ar-th-dollar 'line arg arg))

(defun ar-dollar-list-atpt (&optional arg)
  "Dollar LIST at point."
  (interactive "*p")
  (ar-th-dollar 'list arg arg))

(defun ar-dollar-name-atpt (&optional arg)
  "Dollar NAME at point."
  (interactive "*p")
  (ar-th-dollar 'name arg arg))

(defun ar-dollar-number-atpt (&optional arg)
  "Dollar NUMBER at point."
  (interactive "*p")
  (ar-th-dollar 'number arg arg))

(defun ar-dollar-page-atpt (&optional arg)
  "Dollar PAGE at point."
  (interactive "*p")
  (ar-th-dollar 'page arg arg))

(defun ar-dollar-paragraph-atpt (&optional arg)
  "Dollar PARAGRAPH at point."
  (interactive "*p")
  (ar-th-dollar 'paragraph arg arg))

(defun ar-dollar-phone-atpt (&optional arg)
  "Dollar PHONE at point."
  (interactive "*p")
  (ar-th-dollar 'phone arg arg))

(defun ar-dollar-region-atpt (&optional arg)
  "Dollar REGION at point."
  (interactive "*p")
  (ar-th-dollar 'region arg arg))

(defun ar-dollar-sentence-atpt (&optional arg)
  "Dollar SENTENCE at point."
  (interactive "*p")
  (ar-th-dollar 'sentence arg arg))

(defun ar-dollar-sexp-atpt (&optional arg)
  "Dollar SEXP at point."
  (interactive "*p")
  (ar-th-dollar 'sexp arg arg))

(defun ar-dollar-string-atpt (&optional arg)
  "Dollar STRING at point."
  (interactive "*p")
  (ar-th-dollar 'string arg arg))

(defun ar-dollar-shstruct-atpt (&optional arg)
  "Dollar SHSTRUCT at point."
  (interactive "*p")
  (ar-th-dollar 'shstruct arg arg))

(defun ar-dollar-symbol-atpt (&optional arg)
  "Dollar SYMBOL at point."
  (interactive "*p")
  (ar-th-dollar 'symbol arg arg))

(defun ar-dollar-url-atpt (&optional arg)
  "Dollar URL at point."
  (interactive "*p")
  (ar-th-dollar 'url arg arg))

(defun ar-dollar-word-atpt (&optional arg)
  "Dollar WORD at point."
  (interactive "*p")
  (ar-th-dollar 'word arg arg))

(defun ar-dollar-wordalphaonly-atpt (&optional arg)
  "Dollar WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-dollar 'wordalphaonly arg arg))

(defun ar-doublequoted-greateranglednested-atpt (&optional arg)
  "Doublequoted GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-doublequoted 'greateranglednested arg arg))

(defun ar-doublequoted-lesseranglednested-atpt (&optional arg)
  "Doublequoted LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-doublequoted 'lesseranglednested arg arg))

(defun ar-doublequoted-buffer-atpt (&optional arg)
  "Doublequoted BUFFER at point."
  (interactive "*p")
  (ar-th-doublequoted 'buffer arg arg))

(defun ar-doublequoted-comment-atpt (&optional arg)
  "Doublequoted COMMENT at point."
  (interactive "*p")
  (ar-th-doublequoted 'comment arg arg))

(defun ar-doublequoted-csv-atpt (&optional arg)
  "Doublequoted CSV at point."
  (interactive "*p")
  (ar-th-doublequoted 'csv arg arg))

(defun ar-doublequoted-date-atpt (&optional arg)
  "Doublequoted DATE at point."
  (interactive "*p")
  (ar-th-doublequoted 'date arg arg))

(defun ar-doublequoted-delimited-atpt (&optional arg)
  "Doublequoted DELIMITED at point."
  (interactive "*p")
  (ar-th-doublequoted 'delimited arg arg))

(defun ar-doublequoted-email-atpt (&optional arg)
  "Doublequoted EMAIL at point."
  (interactive "*p")
  (ar-th-doublequoted 'email arg arg))

(defun ar-doublequoted-filename-atpt (&optional arg)
  "Doublequoted FILENAME at point."
  (interactive "*p")
  (ar-th-doublequoted 'filename arg arg))

(defun ar-doublequoted-filenamenondirectory-atpt (&optional arg)
  "Doublequoted FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-doublequoted 'filenamenondirectory arg arg))

(defun ar-doublequoted-float-atpt (&optional arg)
  "Doublequoted FLOAT at point."
  (interactive "*p")
  (ar-th-doublequoted 'float arg arg))

(defun ar-doublequoted-function-atpt (&optional arg)
  "Doublequoted FUNCTION at point."
  (interactive "*p")
  (ar-th-doublequoted 'function arg arg))

(defun ar-doublequoted-ip-atpt (&optional arg)
  "Doublequoted IP at point."
  (interactive "*p")
  (ar-th-doublequoted 'ip arg arg))

(defun ar-doublequoted-isbn-atpt (&optional arg)
  "Doublequoted ISBN at point."
  (interactive "*p")
  (ar-th-doublequoted 'isbn arg arg))

(defun ar-doublequoted-line-atpt (&optional arg)
  "Doublequoted LINE at point."
  (interactive "*p")
  (ar-th-doublequoted 'line arg arg))

(defun ar-doublequoted-list-atpt (&optional arg)
  "Doublequoted LIST at point."
  (interactive "*p")
  (ar-th-doublequoted 'list arg arg))

(defun ar-doublequoted-name-atpt (&optional arg)
  "Doublequoted NAME at point."
  (interactive "*p")
  (ar-th-doublequoted 'name arg arg))

(defun ar-doublequoted-number-atpt (&optional arg)
  "Doublequoted NUMBER at point."
  (interactive "*p")
  (ar-th-doublequoted 'number arg arg))

(defun ar-doublequoted-page-atpt (&optional arg)
  "Doublequoted PAGE at point."
  (interactive "*p")
  (ar-th-doublequoted 'page arg arg))

(defun ar-doublequoted-paragraph-atpt (&optional arg)
  "Doublequoted PARAGRAPH at point."
  (interactive "*p")
  (ar-th-doublequoted 'paragraph arg arg))

(defun ar-doublequoted-phone-atpt (&optional arg)
  "Doublequoted PHONE at point."
  (interactive "*p")
  (ar-th-doublequoted 'phone arg arg))

(defun ar-doublequoted-region-atpt (&optional arg)
  "Doublequoted REGION at point."
  (interactive "*p")
  (ar-th-doublequoted 'region arg arg))

(defun ar-doublequoted-sentence-atpt (&optional arg)
  "Doublequoted SENTENCE at point."
  (interactive "*p")
  (ar-th-doublequoted 'sentence arg arg))

(defun ar-doublequoted-sexp-atpt (&optional arg)
  "Doublequoted SEXP at point."
  (interactive "*p")
  (ar-th-doublequoted 'sexp arg arg))

(defun ar-doublequoted-string-atpt (&optional arg)
  "Doublequoted STRING at point."
  (interactive "*p")
  (ar-th-doublequoted 'string arg arg))

(defun ar-doublequoted-shstruct-atpt (&optional arg)
  "Doublequoted SHSTRUCT at point."
  (interactive "*p")
  (ar-th-doublequoted 'shstruct arg arg))

(defun ar-doublequoted-symbol-atpt (&optional arg)
  "Doublequoted SYMBOL at point."
  (interactive "*p")
  (ar-th-doublequoted 'symbol arg arg))

(defun ar-doublequoted-url-atpt (&optional arg)
  "Doublequoted URL at point."
  (interactive "*p")
  (ar-th-doublequoted 'url arg arg))

(defun ar-doublequoted-word-atpt (&optional arg)
  "Doublequoted WORD at point."
  (interactive "*p")
  (ar-th-doublequoted 'word arg arg))

(defun ar-doublequoted-wordalphaonly-atpt (&optional arg)
  "Doublequoted WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-doublequoted 'wordalphaonly arg arg))

(defun ar-equalized-greateranglednested-atpt (&optional arg)
  "Equalized GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-equalized 'greateranglednested arg arg))

(defun ar-equalized-lesseranglednested-atpt (&optional arg)
  "Equalized LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-equalized 'lesseranglednested arg arg))

(defun ar-equalized-buffer-atpt (&optional arg)
  "Equalized BUFFER at point."
  (interactive "*p")
  (ar-th-equalized 'buffer arg arg))

(defun ar-equalized-comment-atpt (&optional arg)
  "Equalized COMMENT at point."
  (interactive "*p")
  (ar-th-equalized 'comment arg arg))

(defun ar-equalized-csv-atpt (&optional arg)
  "Equalized CSV at point."
  (interactive "*p")
  (ar-th-equalized 'csv arg arg))

(defun ar-equalized-date-atpt (&optional arg)
  "Equalized DATE at point."
  (interactive "*p")
  (ar-th-equalized 'date arg arg))

(defun ar-equalized-delimited-atpt (&optional arg)
  "Equalized DELIMITED at point."
  (interactive "*p")
  (ar-th-equalized 'delimited arg arg))

(defun ar-equalized-email-atpt (&optional arg)
  "Equalized EMAIL at point."
  (interactive "*p")
  (ar-th-equalized 'email arg arg))

(defun ar-equalized-filename-atpt (&optional arg)
  "Equalized FILENAME at point."
  (interactive "*p")
  (ar-th-equalized 'filename arg arg))

(defun ar-equalized-filenamenondirectory-atpt (&optional arg)
  "Equalized FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-equalized 'filenamenondirectory arg arg))

(defun ar-equalized-float-atpt (&optional arg)
  "Equalized FLOAT at point."
  (interactive "*p")
  (ar-th-equalized 'float arg arg))

(defun ar-equalized-function-atpt (&optional arg)
  "Equalized FUNCTION at point."
  (interactive "*p")
  (ar-th-equalized 'function arg arg))

(defun ar-equalized-ip-atpt (&optional arg)
  "Equalized IP at point."
  (interactive "*p")
  (ar-th-equalized 'ip arg arg))

(defun ar-equalized-isbn-atpt (&optional arg)
  "Equalized ISBN at point."
  (interactive "*p")
  (ar-th-equalized 'isbn arg arg))

(defun ar-equalized-line-atpt (&optional arg)
  "Equalized LINE at point."
  (interactive "*p")
  (ar-th-equalized 'line arg arg))

(defun ar-equalized-list-atpt (&optional arg)
  "Equalized LIST at point."
  (interactive "*p")
  (ar-th-equalized 'list arg arg))

(defun ar-equalized-name-atpt (&optional arg)
  "Equalized NAME at point."
  (interactive "*p")
  (ar-th-equalized 'name arg arg))

(defun ar-equalized-number-atpt (&optional arg)
  "Equalized NUMBER at point."
  (interactive "*p")
  (ar-th-equalized 'number arg arg))

(defun ar-equalized-page-atpt (&optional arg)
  "Equalized PAGE at point."
  (interactive "*p")
  (ar-th-equalized 'page arg arg))

(defun ar-equalized-paragraph-atpt (&optional arg)
  "Equalized PARAGRAPH at point."
  (interactive "*p")
  (ar-th-equalized 'paragraph arg arg))

(defun ar-equalized-phone-atpt (&optional arg)
  "Equalized PHONE at point."
  (interactive "*p")
  (ar-th-equalized 'phone arg arg))

(defun ar-equalized-region-atpt (&optional arg)
  "Equalized REGION at point."
  (interactive "*p")
  (ar-th-equalized 'region arg arg))

(defun ar-equalized-sentence-atpt (&optional arg)
  "Equalized SENTENCE at point."
  (interactive "*p")
  (ar-th-equalized 'sentence arg arg))

(defun ar-equalized-sexp-atpt (&optional arg)
  "Equalized SEXP at point."
  (interactive "*p")
  (ar-th-equalized 'sexp arg arg))

(defun ar-equalized-string-atpt (&optional arg)
  "Equalized STRING at point."
  (interactive "*p")
  (ar-th-equalized 'string arg arg))

(defun ar-equalized-shstruct-atpt (&optional arg)
  "Equalized SHSTRUCT at point."
  (interactive "*p")
  (ar-th-equalized 'shstruct arg arg))

(defun ar-equalized-symbol-atpt (&optional arg)
  "Equalized SYMBOL at point."
  (interactive "*p")
  (ar-th-equalized 'symbol arg arg))

(defun ar-equalized-url-atpt (&optional arg)
  "Equalized URL at point."
  (interactive "*p")
  (ar-th-equalized 'url arg arg))

(defun ar-equalized-word-atpt (&optional arg)
  "Equalized WORD at point."
  (interactive "*p")
  (ar-th-equalized 'word arg arg))

(defun ar-equalized-wordalphaonly-atpt (&optional arg)
  "Equalized WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-equalized 'wordalphaonly arg arg))

(defun ar-hash-greateranglednested-atpt (&optional arg)
  "Hash GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-hash 'greateranglednested arg arg))

(defun ar-hash-lesseranglednested-atpt (&optional arg)
  "Hash LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-hash 'lesseranglednested arg arg))

(defun ar-hash-buffer-atpt (&optional arg)
  "Hash BUFFER at point."
  (interactive "*p")
  (ar-th-hash 'buffer arg arg))

(defun ar-hash-comment-atpt (&optional arg)
  "Hash COMMENT at point."
  (interactive "*p")
  (ar-th-hash 'comment arg arg))

(defun ar-hash-csv-atpt (&optional arg)
  "Hash CSV at point."
  (interactive "*p")
  (ar-th-hash 'csv arg arg))

(defun ar-hash-date-atpt (&optional arg)
  "Hash DATE at point."
  (interactive "*p")
  (ar-th-hash 'date arg arg))

(defun ar-hash-delimited-atpt (&optional arg)
  "Hash DELIMITED at point."
  (interactive "*p")
  (ar-th-hash 'delimited arg arg))

(defun ar-hash-email-atpt (&optional arg)
  "Hash EMAIL at point."
  (interactive "*p")
  (ar-th-hash 'email arg arg))

(defun ar-hash-filename-atpt (&optional arg)
  "Hash FILENAME at point."
  (interactive "*p")
  (ar-th-hash 'filename arg arg))

(defun ar-hash-filenamenondirectory-atpt (&optional arg)
  "Hash FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-hash 'filenamenondirectory arg arg))

(defun ar-hash-float-atpt (&optional arg)
  "Hash FLOAT at point."
  (interactive "*p")
  (ar-th-hash 'float arg arg))

(defun ar-hash-function-atpt (&optional arg)
  "Hash FUNCTION at point."
  (interactive "*p")
  (ar-th-hash 'function arg arg))

(defun ar-hash-ip-atpt (&optional arg)
  "Hash IP at point."
  (interactive "*p")
  (ar-th-hash 'ip arg arg))

(defun ar-hash-isbn-atpt (&optional arg)
  "Hash ISBN at point."
  (interactive "*p")
  (ar-th-hash 'isbn arg arg))

(defun ar-hash-line-atpt (&optional arg)
  "Hash LINE at point."
  (interactive "*p")
  (ar-th-hash 'line arg arg))

(defun ar-hash-list-atpt (&optional arg)
  "Hash LIST at point."
  (interactive "*p")
  (ar-th-hash 'list arg arg))

(defun ar-hash-name-atpt (&optional arg)
  "Hash NAME at point."
  (interactive "*p")
  (ar-th-hash 'name arg arg))

(defun ar-hash-number-atpt (&optional arg)
  "Hash NUMBER at point."
  (interactive "*p")
  (ar-th-hash 'number arg arg))

(defun ar-hash-page-atpt (&optional arg)
  "Hash PAGE at point."
  (interactive "*p")
  (ar-th-hash 'page arg arg))

(defun ar-hash-paragraph-atpt (&optional arg)
  "Hash PARAGRAPH at point."
  (interactive "*p")
  (ar-th-hash 'paragraph arg arg))

(defun ar-hash-phone-atpt (&optional arg)
  "Hash PHONE at point."
  (interactive "*p")
  (ar-th-hash 'phone arg arg))

(defun ar-hash-region-atpt (&optional arg)
  "Hash REGION at point."
  (interactive "*p")
  (ar-th-hash 'region arg arg))

(defun ar-hash-sentence-atpt (&optional arg)
  "Hash SENTENCE at point."
  (interactive "*p")
  (ar-th-hash 'sentence arg arg))

(defun ar-hash-sexp-atpt (&optional arg)
  "Hash SEXP at point."
  (interactive "*p")
  (ar-th-hash 'sexp arg arg))

(defun ar-hash-string-atpt (&optional arg)
  "Hash STRING at point."
  (interactive "*p")
  (ar-th-hash 'string arg arg))

(defun ar-hash-shstruct-atpt (&optional arg)
  "Hash SHSTRUCT at point."
  (interactive "*p")
  (ar-th-hash 'shstruct arg arg))

(defun ar-hash-symbol-atpt (&optional arg)
  "Hash SYMBOL at point."
  (interactive "*p")
  (ar-th-hash 'symbol arg arg))

(defun ar-hash-url-atpt (&optional arg)
  "Hash URL at point."
  (interactive "*p")
  (ar-th-hash 'url arg arg))

(defun ar-hash-word-atpt (&optional arg)
  "Hash WORD at point."
  (interactive "*p")
  (ar-th-hash 'word arg arg))

(defun ar-hash-wordalphaonly-atpt (&optional arg)
  "Hash WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-hash 'wordalphaonly arg arg))

(defun ar-hyphen-greateranglednested-atpt (&optional arg)
  "Hyphen GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-hyphen 'greateranglednested arg arg))

(defun ar-hyphen-lesseranglednested-atpt (&optional arg)
  "Hyphen LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-hyphen 'lesseranglednested arg arg))

(defun ar-hyphen-buffer-atpt (&optional arg)
  "Hyphen BUFFER at point."
  (interactive "*p")
  (ar-th-hyphen 'buffer arg arg))

(defun ar-hyphen-comment-atpt (&optional arg)
  "Hyphen COMMENT at point."
  (interactive "*p")
  (ar-th-hyphen 'comment arg arg))

(defun ar-hyphen-csv-atpt (&optional arg)
  "Hyphen CSV at point."
  (interactive "*p")
  (ar-th-hyphen 'csv arg arg))

(defun ar-hyphen-date-atpt (&optional arg)
  "Hyphen DATE at point."
  (interactive "*p")
  (ar-th-hyphen 'date arg arg))

(defun ar-hyphen-delimited-atpt (&optional arg)
  "Hyphen DELIMITED at point."
  (interactive "*p")
  (ar-th-hyphen 'delimited arg arg))

(defun ar-hyphen-email-atpt (&optional arg)
  "Hyphen EMAIL at point."
  (interactive "*p")
  (ar-th-hyphen 'email arg arg))

(defun ar-hyphen-filename-atpt (&optional arg)
  "Hyphen FILENAME at point."
  (interactive "*p")
  (ar-th-hyphen 'filename arg arg))

(defun ar-hyphen-filenamenondirectory-atpt (&optional arg)
  "Hyphen FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-hyphen 'filenamenondirectory arg arg))

(defun ar-hyphen-float-atpt (&optional arg)
  "Hyphen FLOAT at point."
  (interactive "*p")
  (ar-th-hyphen 'float arg arg))

(defun ar-hyphen-function-atpt (&optional arg)
  "Hyphen FUNCTION at point."
  (interactive "*p")
  (ar-th-hyphen 'function arg arg))

(defun ar-hyphen-ip-atpt (&optional arg)
  "Hyphen IP at point."
  (interactive "*p")
  (ar-th-hyphen 'ip arg arg))

(defun ar-hyphen-isbn-atpt (&optional arg)
  "Hyphen ISBN at point."
  (interactive "*p")
  (ar-th-hyphen 'isbn arg arg))

(defun ar-hyphen-line-atpt (&optional arg)
  "Hyphen LINE at point."
  (interactive "*p")
  (ar-th-hyphen 'line arg arg))

(defun ar-hyphen-list-atpt (&optional arg)
  "Hyphen LIST at point."
  (interactive "*p")
  (ar-th-hyphen 'list arg arg))

(defun ar-hyphen-name-atpt (&optional arg)
  "Hyphen NAME at point."
  (interactive "*p")
  (ar-th-hyphen 'name arg arg))

(defun ar-hyphen-number-atpt (&optional arg)
  "Hyphen NUMBER at point."
  (interactive "*p")
  (ar-th-hyphen 'number arg arg))

(defun ar-hyphen-page-atpt (&optional arg)
  "Hyphen PAGE at point."
  (interactive "*p")
  (ar-th-hyphen 'page arg arg))

(defun ar-hyphen-paragraph-atpt (&optional arg)
  "Hyphen PARAGRAPH at point."
  (interactive "*p")
  (ar-th-hyphen 'paragraph arg arg))

(defun ar-hyphen-phone-atpt (&optional arg)
  "Hyphen PHONE at point."
  (interactive "*p")
  (ar-th-hyphen 'phone arg arg))

(defun ar-hyphen-region-atpt (&optional arg)
  "Hyphen REGION at point."
  (interactive "*p")
  (ar-th-hyphen 'region arg arg))

(defun ar-hyphen-sentence-atpt (&optional arg)
  "Hyphen SENTENCE at point."
  (interactive "*p")
  (ar-th-hyphen 'sentence arg arg))

(defun ar-hyphen-sexp-atpt (&optional arg)
  "Hyphen SEXP at point."
  (interactive "*p")
  (ar-th-hyphen 'sexp arg arg))

(defun ar-hyphen-string-atpt (&optional arg)
  "Hyphen STRING at point."
  (interactive "*p")
  (ar-th-hyphen 'string arg arg))

(defun ar-hyphen-shstruct-atpt (&optional arg)
  "Hyphen SHSTRUCT at point."
  (interactive "*p")
  (ar-th-hyphen 'shstruct arg arg))

(defun ar-hyphen-symbol-atpt (&optional arg)
  "Hyphen SYMBOL at point."
  (interactive "*p")
  (ar-th-hyphen 'symbol arg arg))

(defun ar-hyphen-url-atpt (&optional arg)
  "Hyphen URL at point."
  (interactive "*p")
  (ar-th-hyphen 'url arg arg))

(defun ar-hyphen-word-atpt (&optional arg)
  "Hyphen WORD at point."
  (interactive "*p")
  (ar-th-hyphen 'word arg arg))

(defun ar-hyphen-wordalphaonly-atpt (&optional arg)
  "Hyphen WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-hyphen 'wordalphaonly arg arg))

(defun ar-singlequoted-greateranglednested-atpt (&optional arg)
  "Singlequoted GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-singlequoted 'greateranglednested arg arg))

(defun ar-singlequoted-lesseranglednested-atpt (&optional arg)
  "Singlequoted LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-singlequoted 'lesseranglednested arg arg))

(defun ar-singlequoted-buffer-atpt (&optional arg)
  "Singlequoted BUFFER at point."
  (interactive "*p")
  (ar-th-singlequoted 'buffer arg arg))

(defun ar-singlequoted-comment-atpt (&optional arg)
  "Singlequoted COMMENT at point."
  (interactive "*p")
  (ar-th-singlequoted 'comment arg arg))

(defun ar-singlequoted-csv-atpt (&optional arg)
  "Singlequoted CSV at point."
  (interactive "*p")
  (ar-th-singlequoted 'csv arg arg))

(defun ar-singlequoted-date-atpt (&optional arg)
  "Singlequoted DATE at point."
  (interactive "*p")
  (ar-th-singlequoted 'date arg arg))

(defun ar-singlequoted-delimited-atpt (&optional arg)
  "Singlequoted DELIMITED at point."
  (interactive "*p")
  (ar-th-singlequoted 'delimited arg arg))

(defun ar-singlequoted-email-atpt (&optional arg)
  "Singlequoted EMAIL at point."
  (interactive "*p")
  (ar-th-singlequoted 'email arg arg))

(defun ar-singlequoted-filename-atpt (&optional arg)
  "Singlequoted FILENAME at point."
  (interactive "*p")
  (ar-th-singlequoted 'filename arg arg))

(defun ar-singlequoted-filenamenondirectory-atpt (&optional arg)
  "Singlequoted FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-singlequoted 'filenamenondirectory arg arg))

(defun ar-singlequoted-float-atpt (&optional arg)
  "Singlequoted FLOAT at point."
  (interactive "*p")
  (ar-th-singlequoted 'float arg arg))

(defun ar-singlequoted-function-atpt (&optional arg)
  "Singlequoted FUNCTION at point."
  (interactive "*p")
  (ar-th-singlequoted 'function arg arg))

(defun ar-singlequoted-ip-atpt (&optional arg)
  "Singlequoted IP at point."
  (interactive "*p")
  (ar-th-singlequoted 'ip arg arg))

(defun ar-singlequoted-isbn-atpt (&optional arg)
  "Singlequoted ISBN at point."
  (interactive "*p")
  (ar-th-singlequoted 'isbn arg arg))

(defun ar-singlequoted-line-atpt (&optional arg)
  "Singlequoted LINE at point."
  (interactive "*p")
  (ar-th-singlequoted 'line arg arg))

(defun ar-singlequoted-list-atpt (&optional arg)
  "Singlequoted LIST at point."
  (interactive "*p")
  (ar-th-singlequoted 'list arg arg))

(defun ar-singlequoted-name-atpt (&optional arg)
  "Singlequoted NAME at point."
  (interactive "*p")
  (ar-th-singlequoted 'name arg arg))

(defun ar-singlequoted-number-atpt (&optional arg)
  "Singlequoted NUMBER at point."
  (interactive "*p")
  (ar-th-singlequoted 'number arg arg))

(defun ar-singlequoted-page-atpt (&optional arg)
  "Singlequoted PAGE at point."
  (interactive "*p")
  (ar-th-singlequoted 'page arg arg))

(defun ar-singlequoted-paragraph-atpt (&optional arg)
  "Singlequoted PARAGRAPH at point."
  (interactive "*p")
  (ar-th-singlequoted 'paragraph arg arg))

(defun ar-singlequoted-phone-atpt (&optional arg)
  "Singlequoted PHONE at point."
  (interactive "*p")
  (ar-th-singlequoted 'phone arg arg))

(defun ar-singlequoted-region-atpt (&optional arg)
  "Singlequoted REGION at point."
  (interactive "*p")
  (ar-th-singlequoted 'region arg arg))

(defun ar-singlequoted-sentence-atpt (&optional arg)
  "Singlequoted SENTENCE at point."
  (interactive "*p")
  (ar-th-singlequoted 'sentence arg arg))

(defun ar-singlequoted-sexp-atpt (&optional arg)
  "Singlequoted SEXP at point."
  (interactive "*p")
  (ar-th-singlequoted 'sexp arg arg))

(defun ar-singlequoted-string-atpt (&optional arg)
  "Singlequoted STRING at point."
  (interactive "*p")
  (ar-th-singlequoted 'string arg arg))

(defun ar-singlequoted-shstruct-atpt (&optional arg)
  "Singlequoted SHSTRUCT at point."
  (interactive "*p")
  (ar-th-singlequoted 'shstruct arg arg))

(defun ar-singlequoted-symbol-atpt (&optional arg)
  "Singlequoted SYMBOL at point."
  (interactive "*p")
  (ar-th-singlequoted 'symbol arg arg))

(defun ar-singlequoted-url-atpt (&optional arg)
  "Singlequoted URL at point."
  (interactive "*p")
  (ar-th-singlequoted 'url arg arg))

(defun ar-singlequoted-word-atpt (&optional arg)
  "Singlequoted WORD at point."
  (interactive "*p")
  (ar-th-singlequoted 'word arg arg))

(defun ar-singlequoted-wordalphaonly-atpt (&optional arg)
  "Singlequoted WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-singlequoted 'wordalphaonly arg arg))

(defun ar-slash-greateranglednested-atpt (&optional arg)
  "Slash GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-slash 'greateranglednested arg arg))

(defun ar-slash-lesseranglednested-atpt (&optional arg)
  "Slash LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-slash 'lesseranglednested arg arg))

(defun ar-slash-buffer-atpt (&optional arg)
  "Slash BUFFER at point."
  (interactive "*p")
  (ar-th-slash 'buffer arg arg))

(defun ar-slash-comment-atpt (&optional arg)
  "Slash COMMENT at point."
  (interactive "*p")
  (ar-th-slash 'comment arg arg))

(defun ar-slash-csv-atpt (&optional arg)
  "Slash CSV at point."
  (interactive "*p")
  (ar-th-slash 'csv arg arg))

(defun ar-slash-date-atpt (&optional arg)
  "Slash DATE at point."
  (interactive "*p")
  (ar-th-slash 'date arg arg))

(defun ar-slash-delimited-atpt (&optional arg)
  "Slash DELIMITED at point."
  (interactive "*p")
  (ar-th-slash 'delimited arg arg))

(defun ar-slash-email-atpt (&optional arg)
  "Slash EMAIL at point."
  (interactive "*p")
  (ar-th-slash 'email arg arg))

(defun ar-slash-filename-atpt (&optional arg)
  "Slash FILENAME at point."
  (interactive "*p")
  (ar-th-slash 'filename arg arg))

(defun ar-slash-filenamenondirectory-atpt (&optional arg)
  "Slash FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-slash 'filenamenondirectory arg arg))

(defun ar-slash-float-atpt (&optional arg)
  "Slash FLOAT at point."
  (interactive "*p")
  (ar-th-slash 'float arg arg))

(defun ar-slash-function-atpt (&optional arg)
  "Slash FUNCTION at point."
  (interactive "*p")
  (ar-th-slash 'function arg arg))

(defun ar-slash-ip-atpt (&optional arg)
  "Slash IP at point."
  (interactive "*p")
  (ar-th-slash 'ip arg arg))

(defun ar-slash-isbn-atpt (&optional arg)
  "Slash ISBN at point."
  (interactive "*p")
  (ar-th-slash 'isbn arg arg))

(defun ar-slash-line-atpt (&optional arg)
  "Slash LINE at point."
  (interactive "*p")
  (ar-th-slash 'line arg arg))

(defun ar-slash-list-atpt (&optional arg)
  "Slash LIST at point."
  (interactive "*p")
  (ar-th-slash 'list arg arg))

(defun ar-slash-name-atpt (&optional arg)
  "Slash NAME at point."
  (interactive "*p")
  (ar-th-slash 'name arg arg))

(defun ar-slash-number-atpt (&optional arg)
  "Slash NUMBER at point."
  (interactive "*p")
  (ar-th-slash 'number arg arg))

(defun ar-slash-page-atpt (&optional arg)
  "Slash PAGE at point."
  (interactive "*p")
  (ar-th-slash 'page arg arg))

(defun ar-slash-paragraph-atpt (&optional arg)
  "Slash PARAGRAPH at point."
  (interactive "*p")
  (ar-th-slash 'paragraph arg arg))

(defun ar-slash-phone-atpt (&optional arg)
  "Slash PHONE at point."
  (interactive "*p")
  (ar-th-slash 'phone arg arg))

(defun ar-slash-region-atpt (&optional arg)
  "Slash REGION at point."
  (interactive "*p")
  (ar-th-slash 'region arg arg))

(defun ar-slash-sentence-atpt (&optional arg)
  "Slash SENTENCE at point."
  (interactive "*p")
  (ar-th-slash 'sentence arg arg))

(defun ar-slash-sexp-atpt (&optional arg)
  "Slash SEXP at point."
  (interactive "*p")
  (ar-th-slash 'sexp arg arg))

(defun ar-slash-string-atpt (&optional arg)
  "Slash STRING at point."
  (interactive "*p")
  (ar-th-slash 'string arg arg))

(defun ar-slash-shstruct-atpt (&optional arg)
  "Slash SHSTRUCT at point."
  (interactive "*p")
  (ar-th-slash 'shstruct arg arg))

(defun ar-slash-symbol-atpt (&optional arg)
  "Slash SYMBOL at point."
  (interactive "*p")
  (ar-th-slash 'symbol arg arg))

(defun ar-slash-url-atpt (&optional arg)
  "Slash URL at point."
  (interactive "*p")
  (ar-th-slash 'url arg arg))

(defun ar-slash-word-atpt (&optional arg)
  "Slash WORD at point."
  (interactive "*p")
  (ar-th-slash 'word arg arg))

(defun ar-slash-wordalphaonly-atpt (&optional arg)
  "Slash WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-slash 'wordalphaonly arg arg))

(defun ar-star-greateranglednested-atpt (&optional arg)
  "Star GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-star 'greateranglednested arg arg))

(defun ar-star-lesseranglednested-atpt (&optional arg)
  "Star LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-star 'lesseranglednested arg arg))

(defun ar-star-buffer-atpt (&optional arg)
  "Star BUFFER at point."
  (interactive "*p")
  (ar-th-star 'buffer arg arg))

(defun ar-star-comment-atpt (&optional arg)
  "Star COMMENT at point."
  (interactive "*p")
  (ar-th-star 'comment arg arg))

(defun ar-star-csv-atpt (&optional arg)
  "Star CSV at point."
  (interactive "*p")
  (ar-th-star 'csv arg arg))

(defun ar-star-date-atpt (&optional arg)
  "Star DATE at point."
  (interactive "*p")
  (ar-th-star 'date arg arg))

(defun ar-star-delimited-atpt (&optional arg)
  "Star DELIMITED at point."
  (interactive "*p")
  (ar-th-star 'delimited arg arg))

(defun ar-star-email-atpt (&optional arg)
  "Star EMAIL at point."
  (interactive "*p")
  (ar-th-star 'email arg arg))

(defun ar-star-filename-atpt (&optional arg)
  "Star FILENAME at point."
  (interactive "*p")
  (ar-th-star 'filename arg arg))

(defun ar-star-filenamenondirectory-atpt (&optional arg)
  "Star FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-star 'filenamenondirectory arg arg))

(defun ar-star-float-atpt (&optional arg)
  "Star FLOAT at point."
  (interactive "*p")
  (ar-th-star 'float arg arg))

(defun ar-star-function-atpt (&optional arg)
  "Star FUNCTION at point."
  (interactive "*p")
  (ar-th-star 'function arg arg))

(defun ar-star-ip-atpt (&optional arg)
  "Star IP at point."
  (interactive "*p")
  (ar-th-star 'ip arg arg))

(defun ar-star-isbn-atpt (&optional arg)
  "Star ISBN at point."
  (interactive "*p")
  (ar-th-star 'isbn arg arg))

(defun ar-star-line-atpt (&optional arg)
  "Star LINE at point."
  (interactive "*p")
  (ar-th-star 'line arg arg))

(defun ar-star-list-atpt (&optional arg)
  "Star LIST at point."
  (interactive "*p")
  (ar-th-star 'list arg arg))

(defun ar-star-name-atpt (&optional arg)
  "Star NAME at point."
  (interactive "*p")
  (ar-th-star 'name arg arg))

(defun ar-star-number-atpt (&optional arg)
  "Star NUMBER at point."
  (interactive "*p")
  (ar-th-star 'number arg arg))

(defun ar-star-page-atpt (&optional arg)
  "Star PAGE at point."
  (interactive "*p")
  (ar-th-star 'page arg arg))

(defun ar-star-paragraph-atpt (&optional arg)
  "Star PARAGRAPH at point."
  (interactive "*p")
  (ar-th-star 'paragraph arg arg))

(defun ar-star-phone-atpt (&optional arg)
  "Star PHONE at point."
  (interactive "*p")
  (ar-th-star 'phone arg arg))

(defun ar-star-region-atpt (&optional arg)
  "Star REGION at point."
  (interactive "*p")
  (ar-th-star 'region arg arg))

(defun ar-star-sentence-atpt (&optional arg)
  "Star SENTENCE at point."
  (interactive "*p")
  (ar-th-star 'sentence arg arg))

(defun ar-star-sexp-atpt (&optional arg)
  "Star SEXP at point."
  (interactive "*p")
  (ar-th-star 'sexp arg arg))

(defun ar-star-string-atpt (&optional arg)
  "Star STRING at point."
  (interactive "*p")
  (ar-th-star 'string arg arg))

(defun ar-star-shstruct-atpt (&optional arg)
  "Star SHSTRUCT at point."
  (interactive "*p")
  (ar-th-star 'shstruct arg arg))

(defun ar-star-symbol-atpt (&optional arg)
  "Star SYMBOL at point."
  (interactive "*p")
  (ar-th-star 'symbol arg arg))

(defun ar-star-url-atpt (&optional arg)
  "Star URL at point."
  (interactive "*p")
  (ar-th-star 'url arg arg))

(defun ar-star-word-atpt (&optional arg)
  "Star WORD at point."
  (interactive "*p")
  (ar-th-star 'word arg arg))

(defun ar-star-wordalphaonly-atpt (&optional arg)
  "Star WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-star 'wordalphaonly arg arg))

(defun ar-tilded-greateranglednested-atpt (&optional arg)
  "Tilded GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-tilded 'greateranglednested arg arg))

(defun ar-tilded-lesseranglednested-atpt (&optional arg)
  "Tilded LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-tilded 'lesseranglednested arg arg))

(defun ar-tilded-buffer-atpt (&optional arg)
  "Tilded BUFFER at point."
  (interactive "*p")
  (ar-th-tilded 'buffer arg arg))

(defun ar-tilded-comment-atpt (&optional arg)
  "Tilded COMMENT at point."
  (interactive "*p")
  (ar-th-tilded 'comment arg arg))

(defun ar-tilded-csv-atpt (&optional arg)
  "Tilded CSV at point."
  (interactive "*p")
  (ar-th-tilded 'csv arg arg))

(defun ar-tilded-date-atpt (&optional arg)
  "Tilded DATE at point."
  (interactive "*p")
  (ar-th-tilded 'date arg arg))

(defun ar-tilded-delimited-atpt (&optional arg)
  "Tilded DELIMITED at point."
  (interactive "*p")
  (ar-th-tilded 'delimited arg arg))

(defun ar-tilded-email-atpt (&optional arg)
  "Tilded EMAIL at point."
  (interactive "*p")
  (ar-th-tilded 'email arg arg))

(defun ar-tilded-filename-atpt (&optional arg)
  "Tilded FILENAME at point."
  (interactive "*p")
  (ar-th-tilded 'filename arg arg))

(defun ar-tilded-filenamenondirectory-atpt (&optional arg)
  "Tilded FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-tilded 'filenamenondirectory arg arg))

(defun ar-tilded-float-atpt (&optional arg)
  "Tilded FLOAT at point."
  (interactive "*p")
  (ar-th-tilded 'float arg arg))

(defun ar-tilded-function-atpt (&optional arg)
  "Tilded FUNCTION at point."
  (interactive "*p")
  (ar-th-tilded 'function arg arg))

(defun ar-tilded-ip-atpt (&optional arg)
  "Tilded IP at point."
  (interactive "*p")
  (ar-th-tilded 'ip arg arg))

(defun ar-tilded-isbn-atpt (&optional arg)
  "Tilded ISBN at point."
  (interactive "*p")
  (ar-th-tilded 'isbn arg arg))

(defun ar-tilded-line-atpt (&optional arg)
  "Tilded LINE at point."
  (interactive "*p")
  (ar-th-tilded 'line arg arg))

(defun ar-tilded-list-atpt (&optional arg)
  "Tilded LIST at point."
  (interactive "*p")
  (ar-th-tilded 'list arg arg))

(defun ar-tilded-name-atpt (&optional arg)
  "Tilded NAME at point."
  (interactive "*p")
  (ar-th-tilded 'name arg arg))

(defun ar-tilded-number-atpt (&optional arg)
  "Tilded NUMBER at point."
  (interactive "*p")
  (ar-th-tilded 'number arg arg))

(defun ar-tilded-page-atpt (&optional arg)
  "Tilded PAGE at point."
  (interactive "*p")
  (ar-th-tilded 'page arg arg))

(defun ar-tilded-paragraph-atpt (&optional arg)
  "Tilded PARAGRAPH at point."
  (interactive "*p")
  (ar-th-tilded 'paragraph arg arg))

(defun ar-tilded-phone-atpt (&optional arg)
  "Tilded PHONE at point."
  (interactive "*p")
  (ar-th-tilded 'phone arg arg))

(defun ar-tilded-region-atpt (&optional arg)
  "Tilded REGION at point."
  (interactive "*p")
  (ar-th-tilded 'region arg arg))

(defun ar-tilded-sentence-atpt (&optional arg)
  "Tilded SENTENCE at point."
  (interactive "*p")
  (ar-th-tilded 'sentence arg arg))

(defun ar-tilded-sexp-atpt (&optional arg)
  "Tilded SEXP at point."
  (interactive "*p")
  (ar-th-tilded 'sexp arg arg))

(defun ar-tilded-string-atpt (&optional arg)
  "Tilded STRING at point."
  (interactive "*p")
  (ar-th-tilded 'string arg arg))

(defun ar-tilded-shstruct-atpt (&optional arg)
  "Tilded SHSTRUCT at point."
  (interactive "*p")
  (ar-th-tilded 'shstruct arg arg))

(defun ar-tilded-symbol-atpt (&optional arg)
  "Tilded SYMBOL at point."
  (interactive "*p")
  (ar-th-tilded 'symbol arg arg))

(defun ar-tilded-url-atpt (&optional arg)
  "Tilded URL at point."
  (interactive "*p")
  (ar-th-tilded 'url arg arg))

(defun ar-tilded-word-atpt (&optional arg)
  "Tilded WORD at point."
  (interactive "*p")
  (ar-th-tilded 'word arg arg))

(defun ar-tilded-wordalphaonly-atpt (&optional arg)
  "Tilded WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-tilded 'wordalphaonly arg arg))

(defun ar-underscored-greateranglednested-atpt (&optional arg)
  "Underscored GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-underscored 'greateranglednested arg arg))

(defun ar-underscored-lesseranglednested-atpt (&optional arg)
  "Underscored LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-underscored 'lesseranglednested arg arg))

(defun ar-underscored-buffer-atpt (&optional arg)
  "Underscored BUFFER at point."
  (interactive "*p")
  (ar-th-underscored 'buffer arg arg))

(defun ar-underscored-comment-atpt (&optional arg)
  "Underscored COMMENT at point."
  (interactive "*p")
  (ar-th-underscored 'comment arg arg))

(defun ar-underscored-csv-atpt (&optional arg)
  "Underscored CSV at point."
  (interactive "*p")
  (ar-th-underscored 'csv arg arg))

(defun ar-underscored-date-atpt (&optional arg)
  "Underscored DATE at point."
  (interactive "*p")
  (ar-th-underscored 'date arg arg))

(defun ar-underscored-delimited-atpt (&optional arg)
  "Underscored DELIMITED at point."
  (interactive "*p")
  (ar-th-underscored 'delimited arg arg))

(defun ar-underscored-email-atpt (&optional arg)
  "Underscored EMAIL at point."
  (interactive "*p")
  (ar-th-underscored 'email arg arg))

(defun ar-underscored-filename-atpt (&optional arg)
  "Underscored FILENAME at point."
  (interactive "*p")
  (ar-th-underscored 'filename arg arg))

(defun ar-underscored-filenamenondirectory-atpt (&optional arg)
  "Underscored FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-underscored 'filenamenondirectory arg arg))

(defun ar-underscored-float-atpt (&optional arg)
  "Underscored FLOAT at point."
  (interactive "*p")
  (ar-th-underscored 'float arg arg))

(defun ar-underscored-function-atpt (&optional arg)
  "Underscored FUNCTION at point."
  (interactive "*p")
  (ar-th-underscored 'function arg arg))

(defun ar-underscored-ip-atpt (&optional arg)
  "Underscored IP at point."
  (interactive "*p")
  (ar-th-underscored 'ip arg arg))

(defun ar-underscored-isbn-atpt (&optional arg)
  "Underscored ISBN at point."
  (interactive "*p")
  (ar-th-underscored 'isbn arg arg))

(defun ar-underscored-line-atpt (&optional arg)
  "Underscored LINE at point."
  (interactive "*p")
  (ar-th-underscored 'line arg arg))

(defun ar-underscored-list-atpt (&optional arg)
  "Underscored LIST at point."
  (interactive "*p")
  (ar-th-underscored 'list arg arg))

(defun ar-underscored-name-atpt (&optional arg)
  "Underscored NAME at point."
  (interactive "*p")
  (ar-th-underscored 'name arg arg))

(defun ar-underscored-number-atpt (&optional arg)
  "Underscored NUMBER at point."
  (interactive "*p")
  (ar-th-underscored 'number arg arg))

(defun ar-underscored-page-atpt (&optional arg)
  "Underscored PAGE at point."
  (interactive "*p")
  (ar-th-underscored 'page arg arg))

(defun ar-underscored-paragraph-atpt (&optional arg)
  "Underscored PARAGRAPH at point."
  (interactive "*p")
  (ar-th-underscored 'paragraph arg arg))

(defun ar-underscored-phone-atpt (&optional arg)
  "Underscored PHONE at point."
  (interactive "*p")
  (ar-th-underscored 'phone arg arg))

(defun ar-underscored-region-atpt (&optional arg)
  "Underscored REGION at point."
  (interactive "*p")
  (ar-th-underscored 'region arg arg))

(defun ar-underscored-sentence-atpt (&optional arg)
  "Underscored SENTENCE at point."
  (interactive "*p")
  (ar-th-underscored 'sentence arg arg))

(defun ar-underscored-sexp-atpt (&optional arg)
  "Underscored SEXP at point."
  (interactive "*p")
  (ar-th-underscored 'sexp arg arg))

(defun ar-underscored-string-atpt (&optional arg)
  "Underscored STRING at point."
  (interactive "*p")
  (ar-th-underscored 'string arg arg))

(defun ar-underscored-shstruct-atpt (&optional arg)
  "Underscored SHSTRUCT at point."
  (interactive "*p")
  (ar-th-underscored 'shstruct arg arg))

(defun ar-underscored-symbol-atpt (&optional arg)
  "Underscored SYMBOL at point."
  (interactive "*p")
  (ar-th-underscored 'symbol arg arg))

(defun ar-underscored-url-atpt (&optional arg)
  "Underscored URL at point."
  (interactive "*p")
  (ar-th-underscored 'url arg arg))

(defun ar-underscored-word-atpt (&optional arg)
  "Underscored WORD at point."
  (interactive "*p")
  (ar-th-underscored 'word arg arg))

(defun ar-underscored-wordalphaonly-atpt (&optional arg)
  "Underscored WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-underscored 'wordalphaonly arg arg))

(defun ar-whitespaced-greateranglednested-atpt (&optional arg)
  "Whitespaced GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-whitespaced 'greateranglednested arg arg))

(defun ar-whitespaced-lesseranglednested-atpt (&optional arg)
  "Whitespaced LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-whitespaced 'lesseranglednested arg arg))

(defun ar-whitespaced-buffer-atpt (&optional arg)
  "Whitespaced BUFFER at point."
  (interactive "*p")
  (ar-th-whitespaced 'buffer arg arg))

(defun ar-whitespaced-comment-atpt (&optional arg)
  "Whitespaced COMMENT at point."
  (interactive "*p")
  (ar-th-whitespaced 'comment arg arg))

(defun ar-whitespaced-csv-atpt (&optional arg)
  "Whitespaced CSV at point."
  (interactive "*p")
  (ar-th-whitespaced 'csv arg arg))

(defun ar-whitespaced-date-atpt (&optional arg)
  "Whitespaced DATE at point."
  (interactive "*p")
  (ar-th-whitespaced 'date arg arg))

(defun ar-whitespaced-delimited-atpt (&optional arg)
  "Whitespaced DELIMITED at point."
  (interactive "*p")
  (ar-th-whitespaced 'delimited arg arg))

(defun ar-whitespaced-email-atpt (&optional arg)
  "Whitespaced EMAIL at point."
  (interactive "*p")
  (ar-th-whitespaced 'email arg arg))

(defun ar-whitespaced-filename-atpt (&optional arg)
  "Whitespaced FILENAME at point."
  (interactive "*p")
  (ar-th-whitespaced 'filename arg arg))

(defun ar-whitespaced-filenamenondirectory-atpt (&optional arg)
  "Whitespaced FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-whitespaced 'filenamenondirectory arg arg))

(defun ar-whitespaced-float-atpt (&optional arg)
  "Whitespaced FLOAT at point."
  (interactive "*p")
  (ar-th-whitespaced 'float arg arg))

(defun ar-whitespaced-function-atpt (&optional arg)
  "Whitespaced FUNCTION at point."
  (interactive "*p")
  (ar-th-whitespaced 'function arg arg))

(defun ar-whitespaced-ip-atpt (&optional arg)
  "Whitespaced IP at point."
  (interactive "*p")
  (ar-th-whitespaced 'ip arg arg))

(defun ar-whitespaced-isbn-atpt (&optional arg)
  "Whitespaced ISBN at point."
  (interactive "*p")
  (ar-th-whitespaced 'isbn arg arg))

(defun ar-whitespaced-line-atpt (&optional arg)
  "Whitespaced LINE at point."
  (interactive "*p")
  (ar-th-whitespaced 'line arg arg))

(defun ar-whitespaced-list-atpt (&optional arg)
  "Whitespaced LIST at point."
  (interactive "*p")
  (ar-th-whitespaced 'list arg arg))

(defun ar-whitespaced-name-atpt (&optional arg)
  "Whitespaced NAME at point."
  (interactive "*p")
  (ar-th-whitespaced 'name arg arg))

(defun ar-whitespaced-number-atpt (&optional arg)
  "Whitespaced NUMBER at point."
  (interactive "*p")
  (ar-th-whitespaced 'number arg arg))

(defun ar-whitespaced-page-atpt (&optional arg)
  "Whitespaced PAGE at point."
  (interactive "*p")
  (ar-th-whitespaced 'page arg arg))

(defun ar-whitespaced-paragraph-atpt (&optional arg)
  "Whitespaced PARAGRAPH at point."
  (interactive "*p")
  (ar-th-whitespaced 'paragraph arg arg))

(defun ar-whitespaced-phone-atpt (&optional arg)
  "Whitespaced PHONE at point."
  (interactive "*p")
  (ar-th-whitespaced 'phone arg arg))

(defun ar-whitespaced-region-atpt (&optional arg)
  "Whitespaced REGION at point."
  (interactive "*p")
  (ar-th-whitespaced 'region arg arg))

(defun ar-whitespaced-sentence-atpt (&optional arg)
  "Whitespaced SENTENCE at point."
  (interactive "*p")
  (ar-th-whitespaced 'sentence arg arg))

(defun ar-whitespaced-sexp-atpt (&optional arg)
  "Whitespaced SEXP at point."
  (interactive "*p")
  (ar-th-whitespaced 'sexp arg arg))

(defun ar-whitespaced-string-atpt (&optional arg)
  "Whitespaced STRING at point."
  (interactive "*p")
  (ar-th-whitespaced 'string arg arg))

(defun ar-whitespaced-shstruct-atpt (&optional arg)
  "Whitespaced SHSTRUCT at point."
  (interactive "*p")
  (ar-th-whitespaced 'shstruct arg arg))

(defun ar-whitespaced-symbol-atpt (&optional arg)
  "Whitespaced SYMBOL at point."
  (interactive "*p")
  (ar-th-whitespaced 'symbol arg arg))

(defun ar-whitespaced-url-atpt (&optional arg)
  "Whitespaced URL at point."
  (interactive "*p")
  (ar-th-whitespaced 'url arg arg))

(defun ar-whitespaced-word-atpt (&optional arg)
  "Whitespaced WORD at point."
  (interactive "*p")
  (ar-th-whitespaced 'word arg arg))

(defun ar-whitespaced-wordalphaonly-atpt (&optional arg)
  "Whitespaced WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-whitespaced 'wordalphaonly arg arg))

;; ar-thing-at-point-utils-rest-unpaired-delimit ar-unpaired-delimlist-aktiv ar-atpt-rest-list: end

;; ar-thing-at-point-utils-list-delimit ar-unpaired-delimlist-aktiv  ar-paired-delimited-passiv-raw: start
(defun ar-backslashed-(braced "{" "}")-atpt (&optional arg)
  "Backslashed (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "\\" "\\" arg arg))

(defun ar-backslashed-(bracketed "[" "]")-atpt (&optional arg)
  "Backslashed (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "\\" "\\" arg arg))

(defun ar-backslashed-(lesserangled "<" ">")-atpt (&optional arg)
  "Backslashed (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "\\" "\\" arg arg))

(defun ar-backslashed-(greaterangled ">" "<")-atpt (&optional arg)
  "Backslashed (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "\\" "\\" arg arg))

(defun ar-backslashed-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Backslashed (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "\\" "\\" arg arg))

(defun ar-backslashed-(parentized "(" ")")-atpt (&optional arg)
  "Backslashed (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "\\" "\\" arg arg))

(defun ar-backticked-(braced "{" "}")-atpt (&optional arg)
  "Backticked (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "`" "`" arg arg))

(defun ar-backticked-(bracketed "[" "]")-atpt (&optional arg)
  "Backticked (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "`" "`" arg arg))

(defun ar-backticked-(lesserangled "<" ">")-atpt (&optional arg)
  "Backticked (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "`" "`" arg arg))

(defun ar-backticked-(greaterangled ">" "<")-atpt (&optional arg)
  "Backticked (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "`" "`" arg arg))

(defun ar-backticked-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Backticked (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "`" "`" arg arg))

(defun ar-backticked-(parentized "(" ")")-atpt (&optional arg)
  "Backticked (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "`" "`" arg arg))

(defun ar-coloned-(braced "{" "}")-atpt (&optional arg)
  "Coloned (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") ":" ":" arg arg))

(defun ar-coloned-(bracketed "[" "]")-atpt (&optional arg)
  "Coloned (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") ":" ":" arg arg))

(defun ar-coloned-(lesserangled "<" ">")-atpt (&optional arg)
  "Coloned (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") ":" ":" arg arg))

(defun ar-coloned-(greaterangled ">" "<")-atpt (&optional arg)
  "Coloned (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") ":" ":" arg arg))

(defun ar-coloned-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Coloned (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") ":" ":" arg arg))

(defun ar-coloned-(parentized "(" ")")-atpt (&optional arg)
  "Coloned (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") ":" ":" arg arg))

(defun ar-crossed-(braced "{" "}")-atpt (&optional arg)
  "Crossed (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "+" "+" arg arg))

(defun ar-crossed-(bracketed "[" "]")-atpt (&optional arg)
  "Crossed (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "+" "+" arg arg))

(defun ar-crossed-(lesserangled "<" ">")-atpt (&optional arg)
  "Crossed (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "+" "+" arg arg))

(defun ar-crossed-(greaterangled ">" "<")-atpt (&optional arg)
  "Crossed (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "+" "+" arg arg))

(defun ar-crossed-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Crossed (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "+" "+" arg arg))

(defun ar-crossed-(parentized "(" ")")-atpt (&optional arg)
  "Crossed (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "+" "+" arg arg))

(defun ar-dollared-(braced "{" "}")-atpt (&optional arg)
  "Dollared (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "$" "$" arg arg))

(defun ar-dollared-(bracketed "[" "]")-atpt (&optional arg)
  "Dollared (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "$" "$" arg arg))

(defun ar-dollared-(lesserangled "<" ">")-atpt (&optional arg)
  "Dollared (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "$" "$" arg arg))

(defun ar-dollared-(greaterangled ">" "<")-atpt (&optional arg)
  "Dollared (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "$" "$" arg arg))

(defun ar-dollared-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Dollared (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "$" "$" arg arg))

(defun ar-dollared-(parentized "(" ")")-atpt (&optional arg)
  "Dollared (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "$" "$" arg arg))

(defun ar-doublequoted-(braced "{" "}")-atpt (&optional arg)
  "Doublequoted (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "\"" "\"" arg arg))

(defun ar-doublequoted-(bracketed "[" "]")-atpt (&optional arg)
  "Doublequoted (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "\"" "\"" arg arg))

(defun ar-doublequoted-(lesserangled "<" ">")-atpt (&optional arg)
  "Doublequoted (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "\"" "\"" arg arg))

(defun ar-doublequoted-(greaterangled ">" "<")-atpt (&optional arg)
  "Doublequoted (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "\"" "\"" arg arg))

(defun ar-doublequoted-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Doublequoted (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "\"" "\"" arg arg))

(defun ar-doublequoted-(parentized "(" ")")-atpt (&optional arg)
  "Doublequoted (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "\"" "\"" arg arg))

(defun ar-equalized-(braced "{" "}")-atpt (&optional arg)
  "Equalized (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "=" "=" arg arg))

(defun ar-equalized-(bracketed "[" "]")-atpt (&optional arg)
  "Equalized (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "=" "=" arg arg))

(defun ar-equalized-(lesserangled "<" ">")-atpt (&optional arg)
  "Equalized (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "=" "=" arg arg))

(defun ar-equalized-(greaterangled ">" "<")-atpt (&optional arg)
  "Equalized (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "=" "=" arg arg))

(defun ar-equalized-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Equalized (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "=" "=" arg arg))

(defun ar-equalized-(parentized "(" ")")-atpt (&optional arg)
  "Equalized (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "=" "=" arg arg))

(defun ar-hashed-(braced "{" "}")-atpt (&optional arg)
  "Hashed (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "#" "#" arg arg))

(defun ar-hashed-(bracketed "[" "]")-atpt (&optional arg)
  "Hashed (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "#" "#" arg arg))

(defun ar-hashed-(lesserangled "<" ">")-atpt (&optional arg)
  "Hashed (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "#" "#" arg arg))

(defun ar-hashed-(greaterangled ">" "<")-atpt (&optional arg)
  "Hashed (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "#" "#" arg arg))

(defun ar-hashed-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Hashed (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "#" "#" arg arg))

(defun ar-hashed-(parentized "(" ")")-atpt (&optional arg)
  "Hashed (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "#" "#" arg arg))

(defun ar-hyphened-(braced "{" "}")-atpt (&optional arg)
  "Hyphened (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "-" "-" arg arg))

(defun ar-hyphened-(bracketed "[" "]")-atpt (&optional arg)
  "Hyphened (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "-" "-" arg arg))

(defun ar-hyphened-(lesserangled "<" ">")-atpt (&optional arg)
  "Hyphened (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "-" "-" arg arg))

(defun ar-hyphened-(greaterangled ">" "<")-atpt (&optional arg)
  "Hyphened (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "-" "-" arg arg))

(defun ar-hyphened-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Hyphened (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "-" "-" arg arg))

(defun ar-hyphened-(parentized "(" ")")-atpt (&optional arg)
  "Hyphened (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "-" "-" arg arg))

(defun ar-singlequoted-(braced "{" "}")-atpt (&optional arg)
  "Singlequoted (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "'" "'" arg arg))

(defun ar-singlequoted-(bracketed "[" "]")-atpt (&optional arg)
  "Singlequoted (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "'" "'" arg arg))

(defun ar-singlequoted-(lesserangled "<" ">")-atpt (&optional arg)
  "Singlequoted (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "'" "'" arg arg))

(defun ar-singlequoted-(greaterangled ">" "<")-atpt (&optional arg)
  "Singlequoted (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "'" "'" arg arg))

(defun ar-singlequoted-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Singlequoted (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "'" "'" arg arg))

(defun ar-singlequoted-(parentized "(" ")")-atpt (&optional arg)
  "Singlequoted (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "'" "'" arg arg))

(defun ar-slashed-(braced "{" "}")-atpt (&optional arg)
  "Slashed (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "/" "/" arg arg))

(defun ar-slashed-(bracketed "[" "]")-atpt (&optional arg)
  "Slashed (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "/" "/" arg arg))

(defun ar-slashed-(lesserangled "<" ">")-atpt (&optional arg)
  "Slashed (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "/" "/" arg arg))

(defun ar-slashed-(greaterangled ">" "<")-atpt (&optional arg)
  "Slashed (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "/" "/" arg arg))

(defun ar-slashed-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Slashed (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "/" "/" arg arg))

(defun ar-slashed-(parentized "(" ")")-atpt (&optional arg)
  "Slashed (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "/" "/" arg arg))

(defun ar-stared-(braced "{" "}")-atpt (&optional arg)
  "Stared (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "*" "*" arg arg))

(defun ar-stared-(bracketed "[" "]")-atpt (&optional arg)
  "Stared (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "*" "*" arg arg))

(defun ar-stared-(lesserangled "<" ">")-atpt (&optional arg)
  "Stared (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "*" "*" arg arg))

(defun ar-stared-(greaterangled ">" "<")-atpt (&optional arg)
  "Stared (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "*" "*" arg arg))

(defun ar-stared-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Stared (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "*" "*" arg arg))

(defun ar-stared-(parentized "(" ")")-atpt (&optional arg)
  "Stared (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "*" "*" arg arg))

(defun ar-tilded-(braced "{" "}")-atpt (&optional arg)
  "Tilded (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "~" "~" arg arg))

(defun ar-tilded-(bracketed "[" "]")-atpt (&optional arg)
  "Tilded (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "~" "~" arg arg))

(defun ar-tilded-(lesserangled "<" ">")-atpt (&optional arg)
  "Tilded (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "~" "~" arg arg))

(defun ar-tilded-(greaterangled ">" "<")-atpt (&optional arg)
  "Tilded (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "~" "~" arg arg))

(defun ar-tilded-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Tilded (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "~" "~" arg arg))

(defun ar-tilded-(parentized "(" ")")-atpt (&optional arg)
  "Tilded (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "~" "~" arg arg))

(defun ar-underscored-(braced "{" "}")-atpt (&optional arg)
  "Underscored (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") "_" "_" arg arg))

(defun ar-underscored-(bracketed "[" "]")-atpt (&optional arg)
  "Underscored (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") "_" "_" arg arg))

(defun ar-underscored-(lesserangled "<" ">")-atpt (&optional arg)
  "Underscored (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") "_" "_" arg arg))

(defun ar-underscored-(greaterangled ">" "<")-atpt (&optional arg)
  "Underscored (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") "_" "_" arg arg))

(defun ar-underscored-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Underscored (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") "_" "_" arg arg))

(defun ar-underscored-(parentized "(" ")")-atpt (&optional arg)
  "Underscored (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") "_" "_" arg arg))

(defun ar-whitespaced-(braced "{" "}")-atpt (&optional arg)
  "Whitespaced (BRACED "{" "}") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(braced "{" "}") " " " " arg arg))

(defun ar-whitespaced-(bracketed "[" "]")-atpt (&optional arg)
  "Whitespaced (BRACKETED "[" "]") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(bracketed "[" "]") " " " " arg arg))

(defun ar-whitespaced-(lesserangled "<" ">")-atpt (&optional arg)
  "Whitespaced (LESSERANGLED "<" ">") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(lesserangled "<" ">") " " " " arg arg))

(defun ar-whitespaced-(greaterangled ">" "<")-atpt (&optional arg)
  "Whitespaced (GREATERANGLED ">" "<") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(greaterangled ">" "<") " " " " arg arg))

(defun ar-whitespaced-(leftrightsinglequoted "‘" "’")-atpt (&optional arg)
  "Whitespaced (LEFTRIGHTSINGLEQUOTED "‘" "’") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(leftrightsinglequoted "‘" "’") " " " " arg arg))

(defun ar-whitespaced-(parentized "(" ")")-atpt (&optional arg)
  "Whitespaced (PARENTIZED "(" ")") at point."
  (interactive "*p")
  (ar-th-delimit--intern '(parentized "(" ")") " " " " arg arg))

;; ar-thing-at-point-utils-list-delimit ar-unpaired-delimlist-aktiv ar-paired-delimited-passiv-raw: end

;; ar-thing-at-point-utils-rest-paired-delimit ar-paired-delimited-passiv-raw ar-atpt-rest-list: start
(defun ar-brace-greateranglednested-atpt (&optional arg)
  "Brace GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'greateranglednested "{" "}" arg arg))

(defun ar-brace-lesseranglednested-atpt (&optional arg)
  "Brace LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'lesseranglednested "{" "}" arg arg))

(defun ar-brace-buffer-atpt (&optional arg)
  "Brace BUFFER at point."
  (interactive "*p")
  (ar-th-delimit--intern 'buffer "{" "}" arg arg))

(defun ar-brace-comment-atpt (&optional arg)
  "Brace COMMENT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'comment "{" "}" arg arg))

(defun ar-brace-csv-atpt (&optional arg)
  "Brace CSV at point."
  (interactive "*p")
  (ar-th-delimit--intern 'csv "{" "}" arg arg))

(defun ar-brace-date-atpt (&optional arg)
  "Brace DATE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'date "{" "}" arg arg))

(defun ar-brace-delimited-atpt (&optional arg)
  "Brace DELIMITED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'delimited "{" "}" arg arg))

(defun ar-brace-email-atpt (&optional arg)
  "Brace EMAIL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'email "{" "}" arg arg))

(defun ar-brace-filename-atpt (&optional arg)
  "Brace FILENAME at point."
  (interactive "*p")
  (ar-th-delimit--intern 'filename "{" "}" arg arg))

(defun ar-brace-filenamenondirectory-atpt (&optional arg)
  "Brace FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-delimit--intern 'filenamenondirectory "{" "}" arg arg))

(defun ar-brace-float-atpt (&optional arg)
  "Brace FLOAT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'float "{" "}" arg arg))

(defun ar-brace-function-atpt (&optional arg)
  "Brace FUNCTION at point."
  (interactive "*p")
  (ar-th-delimit--intern 'function "{" "}" arg arg))

(defun ar-brace-ip-atpt (&optional arg)
  "Brace IP at point."
  (interactive "*p")
  (ar-th-delimit--intern 'ip "{" "}" arg arg))

(defun ar-brace-isbn-atpt (&optional arg)
  "Brace ISBN at point."
  (interactive "*p")
  (ar-th-delimit--intern 'isbn "{" "}" arg arg))

(defun ar-brace-line-atpt (&optional arg)
  "Brace LINE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'line "{" "}" arg arg))

(defun ar-brace-list-atpt (&optional arg)
  "Brace LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list "{" "}" arg arg))

(defun ar-brace-name-atpt (&optional arg)
  "Brace NAME at point."
  (interactive "*p")
  (ar-th-delimit--intern 'name "{" "}" arg arg))

(defun ar-brace-number-atpt (&optional arg)
  "Brace NUMBER at point."
  (interactive "*p")
  (ar-th-delimit--intern 'number "{" "}" arg arg))

(defun ar-brace-page-atpt (&optional arg)
  "Brace PAGE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'page "{" "}" arg arg))

(defun ar-brace-paragraph-atpt (&optional arg)
  "Brace PARAGRAPH at point."
  (interactive "*p")
  (ar-th-delimit--intern 'paragraph "{" "}" arg arg))

(defun ar-brace-phone-atpt (&optional arg)
  "Brace PHONE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'phone "{" "}" arg arg))

(defun ar-brace-region-atpt (&optional arg)
  "Brace REGION at point."
  (interactive "*p")
  (ar-th-delimit--intern 'region "{" "}" arg arg))

(defun ar-brace-sentence-atpt (&optional arg)
  "Brace SENTENCE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'sentence "{" "}" arg arg))

(defun ar-brace-sexp-atpt (&optional arg)
  "Brace SEXP at point."
  (interactive "*p")
  (ar-th-delimit--intern 'sexp "{" "}" arg arg))

(defun ar-brace-string-atpt (&optional arg)
  "Brace STRING at point."
  (interactive "*p")
  (ar-th-delimit--intern 'string "{" "}" arg arg))

(defun ar-brace-shstruct-atpt (&optional arg)
  "Brace SHSTRUCT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'shstruct "{" "}" arg arg))

(defun ar-brace-symbol-atpt (&optional arg)
  "Brace SYMBOL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'symbol "{" "}" arg arg))

(defun ar-brace-url-atpt (&optional arg)
  "Brace URL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'url "{" "}" arg arg))

(defun ar-brace-word-atpt (&optional arg)
  "Brace WORD at point."
  (interactive "*p")
  (ar-th-delimit--intern 'word "{" "}" arg arg))

(defun ar-brace-wordalphaonly-atpt (&optional arg)
  "Brace WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-delimit--intern 'wordalphaonly "{" "}" arg arg))

(defun ar-bracket-greateranglednested-atpt (&optional arg)
  "Bracket GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'greateranglednested "[" "]" arg arg))

(defun ar-bracket-lesseranglednested-atpt (&optional arg)
  "Bracket LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'lesseranglednested "[" "]" arg arg))

(defun ar-bracket-buffer-atpt (&optional arg)
  "Bracket BUFFER at point."
  (interactive "*p")
  (ar-th-delimit--intern 'buffer "[" "]" arg arg))

(defun ar-bracket-comment-atpt (&optional arg)
  "Bracket COMMENT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'comment "[" "]" arg arg))

(defun ar-bracket-csv-atpt (&optional arg)
  "Bracket CSV at point."
  (interactive "*p")
  (ar-th-delimit--intern 'csv "[" "]" arg arg))

(defun ar-bracket-date-atpt (&optional arg)
  "Bracket DATE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'date "[" "]" arg arg))

(defun ar-bracket-delimited-atpt (&optional arg)
  "Bracket DELIMITED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'delimited "[" "]" arg arg))

(defun ar-bracket-email-atpt (&optional arg)
  "Bracket EMAIL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'email "[" "]" arg arg))

(defun ar-bracket-filename-atpt (&optional arg)
  "Bracket FILENAME at point."
  (interactive "*p")
  (ar-th-delimit--intern 'filename "[" "]" arg arg))

(defun ar-bracket-filenamenondirectory-atpt (&optional arg)
  "Bracket FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-delimit--intern 'filenamenondirectory "[" "]" arg arg))

(defun ar-bracket-float-atpt (&optional arg)
  "Bracket FLOAT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'float "[" "]" arg arg))

(defun ar-bracket-function-atpt (&optional arg)
  "Bracket FUNCTION at point."
  (interactive "*p")
  (ar-th-delimit--intern 'function "[" "]" arg arg))

(defun ar-bracket-ip-atpt (&optional arg)
  "Bracket IP at point."
  (interactive "*p")
  (ar-th-delimit--intern 'ip "[" "]" arg arg))

(defun ar-bracket-isbn-atpt (&optional arg)
  "Bracket ISBN at point."
  (interactive "*p")
  (ar-th-delimit--intern 'isbn "[" "]" arg arg))

(defun ar-bracket-line-atpt (&optional arg)
  "Bracket LINE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'line "[" "]" arg arg))

(defun ar-bracket-list-atpt (&optional arg)
  "Bracket LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list "[" "]" arg arg))

(defun ar-bracket-name-atpt (&optional arg)
  "Bracket NAME at point."
  (interactive "*p")
  (ar-th-delimit--intern 'name "[" "]" arg arg))

(defun ar-bracket-number-atpt (&optional arg)
  "Bracket NUMBER at point."
  (interactive "*p")
  (ar-th-delimit--intern 'number "[" "]" arg arg))

(defun ar-bracket-page-atpt (&optional arg)
  "Bracket PAGE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'page "[" "]" arg arg))

(defun ar-bracket-paragraph-atpt (&optional arg)
  "Bracket PARAGRAPH at point."
  (interactive "*p")
  (ar-th-delimit--intern 'paragraph "[" "]" arg arg))

(defun ar-bracket-phone-atpt (&optional arg)
  "Bracket PHONE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'phone "[" "]" arg arg))

(defun ar-bracket-region-atpt (&optional arg)
  "Bracket REGION at point."
  (interactive "*p")
  (ar-th-delimit--intern 'region "[" "]" arg arg))

(defun ar-bracket-sentence-atpt (&optional arg)
  "Bracket SENTENCE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'sentence "[" "]" arg arg))

(defun ar-bracket-sexp-atpt (&optional arg)
  "Bracket SEXP at point."
  (interactive "*p")
  (ar-th-delimit--intern 'sexp "[" "]" arg arg))

(defun ar-bracket-string-atpt (&optional arg)
  "Bracket STRING at point."
  (interactive "*p")
  (ar-th-delimit--intern 'string "[" "]" arg arg))

(defun ar-bracket-shstruct-atpt (&optional arg)
  "Bracket SHSTRUCT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'shstruct "[" "]" arg arg))

(defun ar-bracket-symbol-atpt (&optional arg)
  "Bracket SYMBOL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'symbol "[" "]" arg arg))

(defun ar-bracket-url-atpt (&optional arg)
  "Bracket URL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'url "[" "]" arg arg))

(defun ar-bracket-word-atpt (&optional arg)
  "Bracket WORD at point."
  (interactive "*p")
  (ar-th-delimit--intern 'word "[" "]" arg arg))

(defun ar-bracket-wordalphaonly-atpt (&optional arg)
  "Bracket WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-delimit--intern 'wordalphaonly "[" "]" arg arg))

(defun ar-lesserangle-greateranglednested-atpt (&optional arg)
  "Lesserangle GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'greateranglednested "<" ">" arg arg))

(defun ar-lesserangle-lesseranglednested-atpt (&optional arg)
  "Lesserangle LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'lesseranglednested "<" ">" arg arg))

(defun ar-lesserangle-buffer-atpt (&optional arg)
  "Lesserangle BUFFER at point."
  (interactive "*p")
  (ar-th-delimit--intern 'buffer "<" ">" arg arg))

(defun ar-lesserangle-comment-atpt (&optional arg)
  "Lesserangle COMMENT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'comment "<" ">" arg arg))

(defun ar-lesserangle-csv-atpt (&optional arg)
  "Lesserangle CSV at point."
  (interactive "*p")
  (ar-th-delimit--intern 'csv "<" ">" arg arg))

(defun ar-lesserangle-date-atpt (&optional arg)
  "Lesserangle DATE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'date "<" ">" arg arg))

(defun ar-lesserangle-delimited-atpt (&optional arg)
  "Lesserangle DELIMITED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'delimited "<" ">" arg arg))

(defun ar-lesserangle-email-atpt (&optional arg)
  "Lesserangle EMAIL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'email "<" ">" arg arg))

(defun ar-lesserangle-filename-atpt (&optional arg)
  "Lesserangle FILENAME at point."
  (interactive "*p")
  (ar-th-delimit--intern 'filename "<" ">" arg arg))

(defun ar-lesserangle-filenamenondirectory-atpt (&optional arg)
  "Lesserangle FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-delimit--intern 'filenamenondirectory "<" ">" arg arg))

(defun ar-lesserangle-float-atpt (&optional arg)
  "Lesserangle FLOAT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'float "<" ">" arg arg))

(defun ar-lesserangle-function-atpt (&optional arg)
  "Lesserangle FUNCTION at point."
  (interactive "*p")
  (ar-th-delimit--intern 'function "<" ">" arg arg))

(defun ar-lesserangle-ip-atpt (&optional arg)
  "Lesserangle IP at point."
  (interactive "*p")
  (ar-th-delimit--intern 'ip "<" ">" arg arg))

(defun ar-lesserangle-isbn-atpt (&optional arg)
  "Lesserangle ISBN at point."
  (interactive "*p")
  (ar-th-delimit--intern 'isbn "<" ">" arg arg))

(defun ar-lesserangle-line-atpt (&optional arg)
  "Lesserangle LINE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'line "<" ">" arg arg))

(defun ar-lesserangle-list-atpt (&optional arg)
  "Lesserangle LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list "<" ">" arg arg))

(defun ar-lesserangle-name-atpt (&optional arg)
  "Lesserangle NAME at point."
  (interactive "*p")
  (ar-th-delimit--intern 'name "<" ">" arg arg))

(defun ar-lesserangle-number-atpt (&optional arg)
  "Lesserangle NUMBER at point."
  (interactive "*p")
  (ar-th-delimit--intern 'number "<" ">" arg arg))

(defun ar-lesserangle-page-atpt (&optional arg)
  "Lesserangle PAGE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'page "<" ">" arg arg))

(defun ar-lesserangle-paragraph-atpt (&optional arg)
  "Lesserangle PARAGRAPH at point."
  (interactive "*p")
  (ar-th-delimit--intern 'paragraph "<" ">" arg arg))

(defun ar-lesserangle-phone-atpt (&optional arg)
  "Lesserangle PHONE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'phone "<" ">" arg arg))

(defun ar-lesserangle-region-atpt (&optional arg)
  "Lesserangle REGION at point."
  (interactive "*p")
  (ar-th-delimit--intern 'region "<" ">" arg arg))

(defun ar-lesserangle-sentence-atpt (&optional arg)
  "Lesserangle SENTENCE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'sentence "<" ">" arg arg))

(defun ar-lesserangle-sexp-atpt (&optional arg)
  "Lesserangle SEXP at point."
  (interactive "*p")
  (ar-th-delimit--intern 'sexp "<" ">" arg arg))

(defun ar-lesserangle-string-atpt (&optional arg)
  "Lesserangle STRING at point."
  (interactive "*p")
  (ar-th-delimit--intern 'string "<" ">" arg arg))

(defun ar-lesserangle-shstruct-atpt (&optional arg)
  "Lesserangle SHSTRUCT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'shstruct "<" ">" arg arg))

(defun ar-lesserangle-symbol-atpt (&optional arg)
  "Lesserangle SYMBOL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'symbol "<" ">" arg arg))

(defun ar-lesserangle-url-atpt (&optional arg)
  "Lesserangle URL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'url "<" ">" arg arg))

(defun ar-lesserangle-word-atpt (&optional arg)
  "Lesserangle WORD at point."
  (interactive "*p")
  (ar-th-delimit--intern 'word "<" ">" arg arg))

(defun ar-lesserangle-wordalphaonly-atpt (&optional arg)
  "Lesserangle WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-delimit--intern 'wordalphaonly "<" ">" arg arg))

(defun ar-greaterangle-greateranglednested-atpt (&optional arg)
  "Greaterangle GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'greateranglednested ">" "<" arg arg))

(defun ar-greaterangle-lesseranglednested-atpt (&optional arg)
  "Greaterangle LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'lesseranglednested ">" "<" arg arg))

(defun ar-greaterangle-buffer-atpt (&optional arg)
  "Greaterangle BUFFER at point."
  (interactive "*p")
  (ar-th-delimit--intern 'buffer ">" "<" arg arg))

(defun ar-greaterangle-comment-atpt (&optional arg)
  "Greaterangle COMMENT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'comment ">" "<" arg arg))

(defun ar-greaterangle-csv-atpt (&optional arg)
  "Greaterangle CSV at point."
  (interactive "*p")
  (ar-th-delimit--intern 'csv ">" "<" arg arg))

(defun ar-greaterangle-date-atpt (&optional arg)
  "Greaterangle DATE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'date ">" "<" arg arg))

(defun ar-greaterangle-delimited-atpt (&optional arg)
  "Greaterangle DELIMITED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'delimited ">" "<" arg arg))

(defun ar-greaterangle-email-atpt (&optional arg)
  "Greaterangle EMAIL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'email ">" "<" arg arg))

(defun ar-greaterangle-filename-atpt (&optional arg)
  "Greaterangle FILENAME at point."
  (interactive "*p")
  (ar-th-delimit--intern 'filename ">" "<" arg arg))

(defun ar-greaterangle-filenamenondirectory-atpt (&optional arg)
  "Greaterangle FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-delimit--intern 'filenamenondirectory ">" "<" arg arg))

(defun ar-greaterangle-float-atpt (&optional arg)
  "Greaterangle FLOAT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'float ">" "<" arg arg))

(defun ar-greaterangle-function-atpt (&optional arg)
  "Greaterangle FUNCTION at point."
  (interactive "*p")
  (ar-th-delimit--intern 'function ">" "<" arg arg))

(defun ar-greaterangle-ip-atpt (&optional arg)
  "Greaterangle IP at point."
  (interactive "*p")
  (ar-th-delimit--intern 'ip ">" "<" arg arg))

(defun ar-greaterangle-isbn-atpt (&optional arg)
  "Greaterangle ISBN at point."
  (interactive "*p")
  (ar-th-delimit--intern 'isbn ">" "<" arg arg))

(defun ar-greaterangle-line-atpt (&optional arg)
  "Greaterangle LINE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'line ">" "<" arg arg))

(defun ar-greaterangle-list-atpt (&optional arg)
  "Greaterangle LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list ">" "<" arg arg))

(defun ar-greaterangle-name-atpt (&optional arg)
  "Greaterangle NAME at point."
  (interactive "*p")
  (ar-th-delimit--intern 'name ">" "<" arg arg))

(defun ar-greaterangle-number-atpt (&optional arg)
  "Greaterangle NUMBER at point."
  (interactive "*p")
  (ar-th-delimit--intern 'number ">" "<" arg arg))

(defun ar-greaterangle-page-atpt (&optional arg)
  "Greaterangle PAGE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'page ">" "<" arg arg))

(defun ar-greaterangle-paragraph-atpt (&optional arg)
  "Greaterangle PARAGRAPH at point."
  (interactive "*p")
  (ar-th-delimit--intern 'paragraph ">" "<" arg arg))

(defun ar-greaterangle-phone-atpt (&optional arg)
  "Greaterangle PHONE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'phone ">" "<" arg arg))

(defun ar-greaterangle-region-atpt (&optional arg)
  "Greaterangle REGION at point."
  (interactive "*p")
  (ar-th-delimit--intern 'region ">" "<" arg arg))

(defun ar-greaterangle-sentence-atpt (&optional arg)
  "Greaterangle SENTENCE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'sentence ">" "<" arg arg))

(defun ar-greaterangle-sexp-atpt (&optional arg)
  "Greaterangle SEXP at point."
  (interactive "*p")
  (ar-th-delimit--intern 'sexp ">" "<" arg arg))

(defun ar-greaterangle-string-atpt (&optional arg)
  "Greaterangle STRING at point."
  (interactive "*p")
  (ar-th-delimit--intern 'string ">" "<" arg arg))

(defun ar-greaterangle-shstruct-atpt (&optional arg)
  "Greaterangle SHSTRUCT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'shstruct ">" "<" arg arg))

(defun ar-greaterangle-symbol-atpt (&optional arg)
  "Greaterangle SYMBOL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'symbol ">" "<" arg arg))

(defun ar-greaterangle-url-atpt (&optional arg)
  "Greaterangle URL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'url ">" "<" arg arg))

(defun ar-greaterangle-word-atpt (&optional arg)
  "Greaterangle WORD at point."
  (interactive "*p")
  (ar-th-delimit--intern 'word ">" "<" arg arg))

(defun ar-greaterangle-wordalphaonly-atpt (&optional arg)
  "Greaterangle WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-delimit--intern 'wordalphaonly ">" "<" arg arg))

(defun ar-leftrightsinglequote-greateranglednested-atpt (&optional arg)
  "Leftrightsinglequote GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'greateranglednested "‘" "’" arg arg))

(defun ar-leftrightsinglequote-lesseranglednested-atpt (&optional arg)
  "Leftrightsinglequote LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'lesseranglednested "‘" "’" arg arg))

(defun ar-leftrightsinglequote-buffer-atpt (&optional arg)
  "Leftrightsinglequote BUFFER at point."
  (interactive "*p")
  (ar-th-delimit--intern 'buffer "‘" "’" arg arg))

(defun ar-leftrightsinglequote-comment-atpt (&optional arg)
  "Leftrightsinglequote COMMENT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'comment "‘" "’" arg arg))

(defun ar-leftrightsinglequote-csv-atpt (&optional arg)
  "Leftrightsinglequote CSV at point."
  (interactive "*p")
  (ar-th-delimit--intern 'csv "‘" "’" arg arg))

(defun ar-leftrightsinglequote-date-atpt (&optional arg)
  "Leftrightsinglequote DATE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'date "‘" "’" arg arg))

(defun ar-leftrightsinglequote-delimited-atpt (&optional arg)
  "Leftrightsinglequote DELIMITED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'delimited "‘" "’" arg arg))

(defun ar-leftrightsinglequote-email-atpt (&optional arg)
  "Leftrightsinglequote EMAIL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'email "‘" "’" arg arg))

(defun ar-leftrightsinglequote-filename-atpt (&optional arg)
  "Leftrightsinglequote FILENAME at point."
  (interactive "*p")
  (ar-th-delimit--intern 'filename "‘" "’" arg arg))

(defun ar-leftrightsinglequote-filenamenondirectory-atpt (&optional arg)
  "Leftrightsinglequote FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-delimit--intern 'filenamenondirectory "‘" "’" arg arg))

(defun ar-leftrightsinglequote-float-atpt (&optional arg)
  "Leftrightsinglequote FLOAT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'float "‘" "’" arg arg))

(defun ar-leftrightsinglequote-function-atpt (&optional arg)
  "Leftrightsinglequote FUNCTION at point."
  (interactive "*p")
  (ar-th-delimit--intern 'function "‘" "’" arg arg))

(defun ar-leftrightsinglequote-ip-atpt (&optional arg)
  "Leftrightsinglequote IP at point."
  (interactive "*p")
  (ar-th-delimit--intern 'ip "‘" "’" arg arg))

(defun ar-leftrightsinglequote-isbn-atpt (&optional arg)
  "Leftrightsinglequote ISBN at point."
  (interactive "*p")
  (ar-th-delimit--intern 'isbn "‘" "’" arg arg))

(defun ar-leftrightsinglequote-line-atpt (&optional arg)
  "Leftrightsinglequote LINE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'line "‘" "’" arg arg))

(defun ar-leftrightsinglequote-list-atpt (&optional arg)
  "Leftrightsinglequote LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list "‘" "’" arg arg))

(defun ar-leftrightsinglequote-name-atpt (&optional arg)
  "Leftrightsinglequote NAME at point."
  (interactive "*p")
  (ar-th-delimit--intern 'name "‘" "’" arg arg))

(defun ar-leftrightsinglequote-number-atpt (&optional arg)
  "Leftrightsinglequote NUMBER at point."
  (interactive "*p")
  (ar-th-delimit--intern 'number "‘" "’" arg arg))

(defun ar-leftrightsinglequote-page-atpt (&optional arg)
  "Leftrightsinglequote PAGE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'page "‘" "’" arg arg))

(defun ar-leftrightsinglequote-paragraph-atpt (&optional arg)
  "Leftrightsinglequote PARAGRAPH at point."
  (interactive "*p")
  (ar-th-delimit--intern 'paragraph "‘" "’" arg arg))

(defun ar-leftrightsinglequote-phone-atpt (&optional arg)
  "Leftrightsinglequote PHONE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'phone "‘" "’" arg arg))

(defun ar-leftrightsinglequote-region-atpt (&optional arg)
  "Leftrightsinglequote REGION at point."
  (interactive "*p")
  (ar-th-delimit--intern 'region "‘" "’" arg arg))

(defun ar-leftrightsinglequote-sentence-atpt (&optional arg)
  "Leftrightsinglequote SENTENCE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'sentence "‘" "’" arg arg))

(defun ar-leftrightsinglequote-sexp-atpt (&optional arg)
  "Leftrightsinglequote SEXP at point."
  (interactive "*p")
  (ar-th-delimit--intern 'sexp "‘" "’" arg arg))

(defun ar-leftrightsinglequote-string-atpt (&optional arg)
  "Leftrightsinglequote STRING at point."
  (interactive "*p")
  (ar-th-delimit--intern 'string "‘" "’" arg arg))

(defun ar-leftrightsinglequote-shstruct-atpt (&optional arg)
  "Leftrightsinglequote SHSTRUCT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'shstruct "‘" "’" arg arg))

(defun ar-leftrightsinglequote-symbol-atpt (&optional arg)
  "Leftrightsinglequote SYMBOL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'symbol "‘" "’" arg arg))

(defun ar-leftrightsinglequote-url-atpt (&optional arg)
  "Leftrightsinglequote URL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'url "‘" "’" arg arg))

(defun ar-leftrightsinglequote-word-atpt (&optional arg)
  "Leftrightsinglequote WORD at point."
  (interactive "*p")
  (ar-th-delimit--intern 'word "‘" "’" arg arg))

(defun ar-leftrightsinglequote-wordalphaonly-atpt (&optional arg)
  "Leftrightsinglequote WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-delimit--intern 'wordalphaonly "‘" "’" arg arg))

(defun ar-parentize-greateranglednested-atpt (&optional arg)
  "Parentize GREATERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'greateranglednested "(" ")" arg arg))

(defun ar-parentize-lesseranglednested-atpt (&optional arg)
  "Parentize LESSERANGLEDNESTED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'lesseranglednested "(" ")" arg arg))

(defun ar-parentize-buffer-atpt (&optional arg)
  "Parentize BUFFER at point."
  (interactive "*p")
  (ar-th-delimit--intern 'buffer "(" ")" arg arg))

(defun ar-parentize-comment-atpt (&optional arg)
  "Parentize COMMENT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'comment "(" ")" arg arg))

(defun ar-parentize-csv-atpt (&optional arg)
  "Parentize CSV at point."
  (interactive "*p")
  (ar-th-delimit--intern 'csv "(" ")" arg arg))

(defun ar-parentize-date-atpt (&optional arg)
  "Parentize DATE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'date "(" ")" arg arg))

(defun ar-parentize-delimited-atpt (&optional arg)
  "Parentize DELIMITED at point."
  (interactive "*p")
  (ar-th-delimit--intern 'delimited "(" ")" arg arg))

(defun ar-parentize-email-atpt (&optional arg)
  "Parentize EMAIL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'email "(" ")" arg arg))

(defun ar-parentize-filename-atpt (&optional arg)
  "Parentize FILENAME at point."
  (interactive "*p")
  (ar-th-delimit--intern 'filename "(" ")" arg arg))

(defun ar-parentize-filenamenondirectory-atpt (&optional arg)
  "Parentize FILENAMENONDIRECTORY at point."
  (interactive "*p")
  (ar-th-delimit--intern 'filenamenondirectory "(" ")" arg arg))

(defun ar-parentize-float-atpt (&optional arg)
  "Parentize FLOAT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'float "(" ")" arg arg))

(defun ar-parentize-function-atpt (&optional arg)
  "Parentize FUNCTION at point."
  (interactive "*p")
  (ar-th-delimit--intern 'function "(" ")" arg arg))

(defun ar-parentize-ip-atpt (&optional arg)
  "Parentize IP at point."
  (interactive "*p")
  (ar-th-delimit--intern 'ip "(" ")" arg arg))

(defun ar-parentize-isbn-atpt (&optional arg)
  "Parentize ISBN at point."
  (interactive "*p")
  (ar-th-delimit--intern 'isbn "(" ")" arg arg))

(defun ar-parentize-line-atpt (&optional arg)
  "Parentize LINE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'line "(" ")" arg arg))

(defun ar-parentize-list-atpt (&optional arg)
  "Parentize LIST at point."
  (interactive "*p")
  (ar-th-delimit--intern 'list "(" ")" arg arg))

(defun ar-parentize-name-atpt (&optional arg)
  "Parentize NAME at point."
  (interactive "*p")
  (ar-th-delimit--intern 'name "(" ")" arg arg))

(defun ar-parentize-number-atpt (&optional arg)
  "Parentize NUMBER at point."
  (interactive "*p")
  (ar-th-delimit--intern 'number "(" ")" arg arg))

(defun ar-parentize-page-atpt (&optional arg)
  "Parentize PAGE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'page "(" ")" arg arg))

(defun ar-parentize-paragraph-atpt (&optional arg)
  "Parentize PARAGRAPH at point."
  (interactive "*p")
  (ar-th-delimit--intern 'paragraph "(" ")" arg arg))

(defun ar-parentize-phone-atpt (&optional arg)
  "Parentize PHONE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'phone "(" ")" arg arg))

(defun ar-parentize-region-atpt (&optional arg)
  "Parentize REGION at point."
  (interactive "*p")
  (ar-th-delimit--intern 'region "(" ")" arg arg))

(defun ar-parentize-sentence-atpt (&optional arg)
  "Parentize SENTENCE at point."
  (interactive "*p")
  (ar-th-delimit--intern 'sentence "(" ")" arg arg))

(defun ar-parentize-sexp-atpt (&optional arg)
  "Parentize SEXP at point."
  (interactive "*p")
  (ar-th-delimit--intern 'sexp "(" ")" arg arg))

(defun ar-parentize-string-atpt (&optional arg)
  "Parentize STRING at point."
  (interactive "*p")
  (ar-th-delimit--intern 'string "(" ")" arg arg))

(defun ar-parentize-shstruct-atpt (&optional arg)
  "Parentize SHSTRUCT at point."
  (interactive "*p")
  (ar-th-delimit--intern 'shstruct "(" ")" arg arg))

(defun ar-parentize-symbol-atpt (&optional arg)
  "Parentize SYMBOL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'symbol "(" ")" arg arg))

(defun ar-parentize-url-atpt (&optional arg)
  "Parentize URL at point."
  (interactive "*p")
  (ar-th-delimit--intern 'url "(" ")" arg arg))

(defun ar-parentize-word-atpt (&optional arg)
  "Parentize WORD at point."
  (interactive "*p")
  (ar-th-delimit--intern 'word "(" ")" arg arg))

(defun ar-parentize-wordalphaonly-atpt (&optional arg)
  "Parentize WORDALPHAONLY at point."
  (interactive "*p")
  (ar-th-delimit--intern 'wordalphaonly "(" ")" arg arg))

;; ar-thing-at-point-utils-rest-paired-delimit ar-paired-delimited-passiv-raw ar-atpt-rest-list: end

;; ar-thing-at-point-utils-activ-passiv ar-paired-delim-aktiv-raw ar-paired-delimited-passiv-raw: start
(defun ar-brace-braced-atpt ()
  "Brace BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'brace "{" "}"))

(defun ar-bracket-braced-atpt ()
  "Bracket BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'bracket "{" "}"))

(defun ar-lesserangle-braced-atpt ()
  "Lesserangle BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'lesserangle "{" "}"))

(defun ar-greaterangle-braced-atpt ()
  "Greaterangle BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'greaterangle "{" "}"))

(defun ar-leftrightsinglequote-braced-atpt ()
  "Leftrightsinglequote BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'leftrightsinglequote "{" "}"))

(defun ar-leftrightsinglequote-braced-atpt ()
  "Leftrightsinglequote BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'leftrightsinglequote "{" "}"))

(defun ar-parentize-braced-atpt ()
  "Parentize BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'parentize "{" "}"))

(defun ar-brace-bracketed-atpt ()
  "Brace BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'brace "[" "]"))

(defun ar-bracket-bracketed-atpt ()
  "Bracket BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'bracket "[" "]"))

(defun ar-lesserangle-bracketed-atpt ()
  "Lesserangle BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'lesserangle "[" "]"))

(defun ar-greaterangle-bracketed-atpt ()
  "Greaterangle BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'greaterangle "[" "]"))

(defun ar-leftrightsinglequote-bracketed-atpt ()
  "Leftrightsinglequote BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'leftrightsinglequote "[" "]"))

(defun ar-leftrightsinglequote-bracketed-atpt ()
  "Leftrightsinglequote BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'leftrightsinglequote "[" "]"))

(defun ar-parentize-bracketed-atpt ()
  "Parentize BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'parentize "[" "]"))

(defun ar-brace-lesserangled-atpt ()
  "Brace LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'brace "<" ">"))

(defun ar-bracket-lesserangled-atpt ()
  "Bracket LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'bracket "<" ">"))

(defun ar-lesserangle-lesserangled-atpt ()
  "Lesserangle LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'lesserangle "<" ">"))

(defun ar-greaterangle-lesserangled-atpt ()
  "Greaterangle LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'greaterangle "<" ">"))

(defun ar-leftrightsinglequote-lesserangled-atpt ()
  "Leftrightsinglequote LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'leftrightsinglequote "<" ">"))

(defun ar-leftrightsinglequote-lesserangled-atpt ()
  "Leftrightsinglequote LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'leftrightsinglequote "<" ">"))

(defun ar-parentize-lesserangled-atpt ()
  "Parentize LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'parentize "<" ">"))

(defun ar-brace-greaterangled-atpt ()
  "Brace GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'brace ">" "<"))

(defun ar-bracket-greaterangled-atpt ()
  "Bracket GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'bracket ">" "<"))

(defun ar-lesserangle-greaterangled-atpt ()
  "Lesserangle GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'lesserangle ">" "<"))

(defun ar-greaterangle-greaterangled-atpt ()
  "Greaterangle GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'greaterangle ">" "<"))

(defun ar-leftrightsinglequote-greaterangled-atpt ()
  "Leftrightsinglequote GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'leftrightsinglequote ">" "<"))

(defun ar-leftrightsinglequote-greaterangled-atpt ()
  "Leftrightsinglequote GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'leftrightsinglequote ">" "<"))

(defun ar-parentize-greaterangled-atpt ()
  "Parentize GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'parentize ">" "<"))

(defun ar-brace-leftrightsinglequoted-atpt ()
  "Brace LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'brace "‘" "’"))

(defun ar-bracket-leftrightsinglequoted-atpt ()
  "Bracket LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'bracket "‘" "’"))

(defun ar-lesserangle-leftrightsinglequoted-atpt ()
  "Lesserangle LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'lesserangle "‘" "’"))

(defun ar-greaterangle-leftrightsinglequoted-atpt ()
  "Greaterangle LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'greaterangle "‘" "’"))

(defun ar-leftrightsinglequote-leftrightsinglequoted-atpt ()
  "Leftrightsinglequote LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'leftrightsinglequote "‘" "’"))

(defun ar-leftrightsinglequote-leftrightsinglequoted-atpt ()
  "Leftrightsinglequote LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'leftrightsinglequote "‘" "’"))

(defun ar-parentize-leftrightsinglequoted-atpt ()
  "Parentize LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'parentize "‘" "’"))

(defun ar-brace-parentized-atpt ()
  "Brace PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'brace "(" ")"))

(defun ar-bracket-parentized-atpt ()
  "Bracket PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'bracket "(" ")"))

(defun ar-lesserangle-parentized-atpt ()
  "Lesserangle PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'lesserangle "(" ")"))

(defun ar-greaterangle-parentized-atpt ()
  "Greaterangle PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'greaterangle "(" ")"))

(defun ar-leftrightsinglequote-parentized-atpt ()
  "Leftrightsinglequote PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'leftrightsinglequote "(" ")"))

(defun ar-leftrightsinglequote-parentized-atpt ()
  "Leftrightsinglequote PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'leftrightsinglequote "(" ")"))

(defun ar-parentize-parentized-atpt ()
  "Parentize PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'parentize "(" ")"))

;; ar-thing-at-point-utils-activ-passiv ar-paired-delim-aktiv-raw ar-paired-delimited-passiv-raw: end
(defun ar-backslash-braced-atpt ()
  "Backslash BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'backslash "{" "}"))

(defun ar-backtick-braced-atpt ()
  "Backtick BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'backtick "{" "}"))

(defun ar-colon-braced-atpt ()
  "Colon BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'colon "{" "}"))

(defun ar-cross-braced-atpt ()
  "Cross BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'cross "{" "}"))

(defun ar-dollar-braced-atpt ()
  "Dollar BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'dollar "{" "}"))

(defun ar-doublequote-braced-atpt ()
  "Doublequote BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'doublequote "{" "}"))

(defun ar-equalize-braced-atpt ()
  "Equalize BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'equalize "{" "}"))

(defun ar-escape-braced-atpt ()
  "Escape BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'escape "{" "}"))

(defun ar-hash-braced-atpt ()
  "Hash BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'hash "{" "}"))

(defun ar-hyphen-braced-atpt ()
  "Hyphen BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'hyphen "{" "}"))

(defun ar-singlequote-braced-atpt ()
  "Singlequote BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'singlequote "{" "}"))

(defun ar-slash-braced-atpt ()
  "Slash BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'slash "{" "}"))

(defun ar-star-braced-atpt ()
  "Star BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'star "{" "}"))

(defun ar-tild-braced-atpt ()
  "Tild BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'tild "{" "}"))

(defun ar-underscore-braced-atpt ()
  "Underscore BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'underscore "{" "}"))

(defun ar-whitespace-braced-atpt ()
  "Whitespace BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'whitespace "{" "}"))

(defun ar-doubleslash-braced-atpt ()
  "Doubleslash BRACED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'doubleslash "{" "}"))

(defun ar-backslash-bracketed-atpt ()
  "Backslash BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'backslash "[" "]"))

(defun ar-backtick-bracketed-atpt ()
  "Backtick BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'backtick "[" "]"))

(defun ar-colon-bracketed-atpt ()
  "Colon BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'colon "[" "]"))

(defun ar-cross-bracketed-atpt ()
  "Cross BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'cross "[" "]"))

(defun ar-dollar-bracketed-atpt ()
  "Dollar BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'dollar "[" "]"))

(defun ar-doublequote-bracketed-atpt ()
  "Doublequote BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'doublequote "[" "]"))

(defun ar-equalize-bracketed-atpt ()
  "Equalize BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'equalize "[" "]"))

(defun ar-escape-bracketed-atpt ()
  "Escape BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'escape "[" "]"))

(defun ar-hash-bracketed-atpt ()
  "Hash BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'hash "[" "]"))

(defun ar-hyphen-bracketed-atpt ()
  "Hyphen BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'hyphen "[" "]"))

(defun ar-singlequote-bracketed-atpt ()
  "Singlequote BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'singlequote "[" "]"))

(defun ar-slash-bracketed-atpt ()
  "Slash BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'slash "[" "]"))

(defun ar-star-bracketed-atpt ()
  "Star BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'star "[" "]"))

(defun ar-tild-bracketed-atpt ()
  "Tild BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'tild "[" "]"))

(defun ar-underscore-bracketed-atpt ()
  "Underscore BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'underscore "[" "]"))

(defun ar-whitespace-bracketed-atpt ()
  "Whitespace BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'whitespace "[" "]"))

(defun ar-doubleslash-bracketed-atpt ()
  "Doubleslash BRACKETED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'doubleslash "[" "]"))

(defun ar-backslash-lesserangled-atpt ()
  "Backslash LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'backslash "<" ">"))

(defun ar-backtick-lesserangled-atpt ()
  "Backtick LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'backtick "<" ">"))

(defun ar-colon-lesserangled-atpt ()
  "Colon LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'colon "<" ">"))

(defun ar-cross-lesserangled-atpt ()
  "Cross LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'cross "<" ">"))

(defun ar-dollar-lesserangled-atpt ()
  "Dollar LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'dollar "<" ">"))

(defun ar-doublequote-lesserangled-atpt ()
  "Doublequote LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'doublequote "<" ">"))

(defun ar-equalize-lesserangled-atpt ()
  "Equalize LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'equalize "<" ">"))

(defun ar-escape-lesserangled-atpt ()
  "Escape LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'escape "<" ">"))

(defun ar-hash-lesserangled-atpt ()
  "Hash LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'hash "<" ">"))

(defun ar-hyphen-lesserangled-atpt ()
  "Hyphen LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'hyphen "<" ">"))

(defun ar-singlequote-lesserangled-atpt ()
  "Singlequote LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'singlequote "<" ">"))

(defun ar-slash-lesserangled-atpt ()
  "Slash LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'slash "<" ">"))

(defun ar-star-lesserangled-atpt ()
  "Star LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'star "<" ">"))

(defun ar-tild-lesserangled-atpt ()
  "Tild LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'tild "<" ">"))

(defun ar-underscore-lesserangled-atpt ()
  "Underscore LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'underscore "<" ">"))

(defun ar-whitespace-lesserangled-atpt ()
  "Whitespace LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'whitespace "<" ">"))

(defun ar-doubleslash-lesserangled-atpt ()
  "Doubleslash LESSERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'doubleslash "<" ">"))

(defun ar-backslash-greaterangled-atpt ()
  "Backslash GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'backslash ">" "<"))

(defun ar-backtick-greaterangled-atpt ()
  "Backtick GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'backtick ">" "<"))

(defun ar-colon-greaterangled-atpt ()
  "Colon GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'colon ">" "<"))

(defun ar-cross-greaterangled-atpt ()
  "Cross GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'cross ">" "<"))

(defun ar-dollar-greaterangled-atpt ()
  "Dollar GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'dollar ">" "<"))

(defun ar-doublequote-greaterangled-atpt ()
  "Doublequote GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'doublequote ">" "<"))

(defun ar-equalize-greaterangled-atpt ()
  "Equalize GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'equalize ">" "<"))

(defun ar-escape-greaterangled-atpt ()
  "Escape GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'escape ">" "<"))

(defun ar-hash-greaterangled-atpt ()
  "Hash GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'hash ">" "<"))

(defun ar-hyphen-greaterangled-atpt ()
  "Hyphen GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'hyphen ">" "<"))

(defun ar-singlequote-greaterangled-atpt ()
  "Singlequote GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'singlequote ">" "<"))

(defun ar-slash-greaterangled-atpt ()
  "Slash GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'slash ">" "<"))

(defun ar-star-greaterangled-atpt ()
  "Star GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'star ">" "<"))

(defun ar-tild-greaterangled-atpt ()
  "Tild GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'tild ">" "<"))

(defun ar-underscore-greaterangled-atpt ()
  "Underscore GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'underscore ">" "<"))

(defun ar-whitespace-greaterangled-atpt ()
  "Whitespace GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'whitespace ">" "<"))

(defun ar-doubleslash-greaterangled-atpt ()
  "Doubleslash GREATERANGLED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'doubleslash ">" "<"))

(defun ar-backslash-leftrightsinglequoted-atpt ()
  "Backslash LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'backslash "‘" "’"))

(defun ar-backtick-leftrightsinglequoted-atpt ()
  "Backtick LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'backtick "‘" "’"))

(defun ar-colon-leftrightsinglequoted-atpt ()
  "Colon LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'colon "‘" "’"))

(defun ar-cross-leftrightsinglequoted-atpt ()
  "Cross LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'cross "‘" "’"))

(defun ar-dollar-leftrightsinglequoted-atpt ()
  "Dollar LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'dollar "‘" "’"))

(defun ar-doublequote-leftrightsinglequoted-atpt ()
  "Doublequote LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'doublequote "‘" "’"))

(defun ar-equalize-leftrightsinglequoted-atpt ()
  "Equalize LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'equalize "‘" "’"))

(defun ar-escape-leftrightsinglequoted-atpt ()
  "Escape LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'escape "‘" "’"))

(defun ar-hash-leftrightsinglequoted-atpt ()
  "Hash LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'hash "‘" "’"))

(defun ar-hyphen-leftrightsinglequoted-atpt ()
  "Hyphen LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'hyphen "‘" "’"))

(defun ar-singlequote-leftrightsinglequoted-atpt ()
  "Singlequote LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'singlequote "‘" "’"))

(defun ar-slash-leftrightsinglequoted-atpt ()
  "Slash LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'slash "‘" "’"))

(defun ar-star-leftrightsinglequoted-atpt ()
  "Star LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'star "‘" "’"))

(defun ar-tild-leftrightsinglequoted-atpt ()
  "Tild LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'tild "‘" "’"))

(defun ar-underscore-leftrightsinglequoted-atpt ()
  "Underscore LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'underscore "‘" "’"))

(defun ar-whitespace-leftrightsinglequoted-atpt ()
  "Whitespace LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'whitespace "‘" "’"))

(defun ar-doubleslash-leftrightsinglequoted-atpt ()
  "Doubleslash LEFTRIGHTSINGLEQUOTED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'doubleslash "‘" "’"))

(defun ar-backslash-parentized-atpt ()
  "Backslash PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'backslash "(" ")"))

(defun ar-backtick-parentized-atpt ()
  "Backtick PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'backtick "(" ")"))

(defun ar-colon-parentized-atpt ()
  "Colon PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'colon "(" ")"))

(defun ar-cross-parentized-atpt ()
  "Cross PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'cross "(" ")"))

(defun ar-dollar-parentized-atpt ()
  "Dollar PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'dollar "(" ")"))

(defun ar-doublequote-parentized-atpt ()
  "Doublequote PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'doublequote "(" ")"))

(defun ar-equalize-parentized-atpt ()
  "Equalize PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'equalize "(" ")"))

(defun ar-escape-parentized-atpt ()
  "Escape PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'escape "(" ")"))

(defun ar-hash-parentized-atpt ()
  "Hash PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'hash "(" ")"))

(defun ar-hyphen-parentized-atpt ()
  "Hyphen PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'hyphen "(" ")"))

(defun ar-singlequote-parentized-atpt ()
  "Singlequote PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'singlequote "(" ")"))

(defun ar-slash-parentized-atpt ()
  "Slash PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'slash "(" ")"))

(defun ar-star-parentized-atpt ()
  "Star PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'star "(" ")"))

(defun ar-tild-parentized-atpt ()
  "Tild PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'tild "(" ")"))

(defun ar-underscore-parentized-atpt ()
  "Underscore PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'underscore "(" ")"))

(defun ar-whitespace-parentized-atpt ()
  "Whitespace PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'whitespace "(" ")"))

(defun ar-doubleslash-parentized-atpt ()
  "Doubleslash PARENTIZED at point."
  (interactive "*")
  ;; benötigt die Begrenzerzeichen
  (ar-th-delimit--intern 'doubleslash "(" ")"))


;; ar-thing-at-point-utils-nodelim-core ar-atpt-classes: start

(defun ar-alnum-atpt ()
  "Returns alnum at point if any, nil otherwise. "
  (interactive)
  (ar-th 'alnum nil nil (interactive-p)))

(defalias 'ar-bounds-of-alnum-atpt 'ar-alnum-bounds-atpt)
(defun ar-alnum-bounds-atpt ()
  "Returns a list, borders of alnum if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'alnum nil (interactive-p)))

(defun ar-alnum-beginning-position-atpt ()
  "Returns a number, beginning position ALNUM at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'alnum nil (interactive-p)))

(defun ar-alnum-end-position-atpt ()
  "Returns a number, end position of ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'alnum nil (interactive-p)))

(defun ar-alnum-beginning-atpt ()
  "Goto beginning of symbol or char-class ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'alnum nil (interactive-p)))

(defun ar-alnum-end-atpt ()
  "Goto end of symbol or char-class ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'alnum nil (interactive-p)))

(defun ar-in-alnum-p-atpt ()
  "Returns bounds of ALNUM at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'alnum nil (interactive-p)))

(defun ar-length-of-alnum-atpt ()
  "Returns beginning of symbol or char-class ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'alnum nil (interactive-p)))

(defun ar-copy-alnum-atpt ()
  "Returns a copy of ALNUM at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'alnum nil (interactive-p)))

(defun ar-delete-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'alnum arg (interactive-p)))

(defun ar-forward-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'alnum arg (interactive-p)))

(defun ar-backward-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'alnum arg (interactive-p)))

(defun ar-triplequotedq-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'alnum arg (interactive-p)))

(defun ar-triplequotesq-alnum-atpt (&optional arg)
  "Deletes alnum at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'alnum arg (interactive-p)))

(defun ar-delete-alnum-in-region (beg end)
  "Deletes ALNUM at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'alnum beg end (interactive-p)))
(defun ar-alpha-atpt ()
  "Returns alpha at point if any, nil otherwise. "
  (interactive)
  (ar-th 'alpha nil nil (interactive-p)))

(defalias 'ar-bounds-of-alpha-atpt 'ar-alpha-bounds-atpt)
(defun ar-alpha-bounds-atpt ()
  "Returns a list, borders of alpha if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'alpha nil (interactive-p)))

(defun ar-alpha-beginning-position-atpt ()
  "Returns a number, beginning position ALPHA at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'alpha nil (interactive-p)))

(defun ar-alpha-end-position-atpt ()
  "Returns a number, end position of ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'alpha nil (interactive-p)))

(defun ar-alpha-beginning-atpt ()
  "Goto beginning of symbol or char-class ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'alpha nil (interactive-p)))

(defun ar-alpha-end-atpt ()
  "Goto end of symbol or char-class ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'alpha nil (interactive-p)))

(defun ar-in-alpha-p-atpt ()
  "Returns bounds of ALPHA at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'alpha nil (interactive-p)))

(defun ar-length-of-alpha-atpt ()
  "Returns beginning of symbol or char-class ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'alpha nil (interactive-p)))

(defun ar-copy-alpha-atpt ()
  "Returns a copy of ALPHA at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'alpha nil (interactive-p)))

(defun ar-delete-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'alpha arg (interactive-p)))

(defun ar-forward-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'alpha arg (interactive-p)))

(defun ar-backward-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'alpha arg (interactive-p)))

(defun ar-triplequotedq-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'alpha arg (interactive-p)))

(defun ar-triplequotesq-alpha-atpt (&optional arg)
  "Deletes alpha at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'alpha arg (interactive-p)))

(defun ar-delete-alpha-in-region (beg end)
  "Deletes ALPHA at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'alpha beg end (interactive-p)))
(defun ar-ascii-atpt ()
  "Returns ascii at point if any, nil otherwise. "
  (interactive)
  (ar-th 'ascii nil nil (interactive-p)))

(defalias 'ar-bounds-of-ascii-atpt 'ar-ascii-bounds-atpt)
(defun ar-ascii-bounds-atpt ()
  "Returns a list, borders of ascii if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'ascii nil (interactive-p)))

(defun ar-ascii-beginning-position-atpt ()
  "Returns a number, beginning position ASCII at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'ascii nil (interactive-p)))

(defun ar-ascii-end-position-atpt ()
  "Returns a number, end position of ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'ascii nil (interactive-p)))

(defun ar-ascii-beginning-atpt ()
  "Goto beginning of symbol or char-class ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'ascii nil (interactive-p)))

(defun ar-ascii-end-atpt ()
  "Goto end of symbol or char-class ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'ascii nil (interactive-p)))

(defun ar-in-ascii-p-atpt ()
  "Returns bounds of ASCII at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'ascii nil (interactive-p)))

(defun ar-length-of-ascii-atpt ()
  "Returns beginning of symbol or char-class ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'ascii nil (interactive-p)))

(defun ar-copy-ascii-atpt ()
  "Returns a copy of ASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'ascii nil (interactive-p)))

(defun ar-delete-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'ascii arg (interactive-p)))

(defun ar-forward-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'ascii arg (interactive-p)))

(defun ar-backward-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'ascii arg (interactive-p)))

(defun ar-triplequotedq-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'ascii arg (interactive-p)))

(defun ar-triplequotesq-ascii-atpt (&optional arg)
  "Deletes ascii at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'ascii arg (interactive-p)))

(defun ar-delete-ascii-in-region (beg end)
  "Deletes ASCII at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'ascii beg end (interactive-p)))
(defun ar-blank-atpt ()
  "Returns blank at point if any, nil otherwise. "
  (interactive)
  (ar-th 'blank nil nil (interactive-p)))

(defalias 'ar-bounds-of-blank-atpt 'ar-blank-bounds-atpt)
(defun ar-blank-bounds-atpt ()
  "Returns a list, borders of blank if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'blank nil (interactive-p)))

(defun ar-blank-beginning-position-atpt ()
  "Returns a number, beginning position BLANK at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'blank nil (interactive-p)))

(defun ar-blank-end-position-atpt ()
  "Returns a number, end position of BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'blank nil (interactive-p)))

(defun ar-blank-beginning-atpt ()
  "Goto beginning of symbol or char-class BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'blank nil (interactive-p)))

(defun ar-blank-end-atpt ()
  "Goto end of symbol or char-class BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'blank nil (interactive-p)))

(defun ar-in-blank-p-atpt ()
  "Returns bounds of BLANK at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'blank nil (interactive-p)))

(defun ar-length-of-blank-atpt ()
  "Returns beginning of symbol or char-class BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'blank nil (interactive-p)))

(defun ar-copy-blank-atpt ()
  "Returns a copy of BLANK at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'blank nil (interactive-p)))

(defun ar-delete-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'blank arg (interactive-p)))

(defun ar-forward-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'blank arg (interactive-p)))

(defun ar-backward-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'blank arg (interactive-p)))

(defun ar-triplequotedq-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'blank arg (interactive-p)))

(defun ar-triplequotesq-blank-atpt (&optional arg)
  "Deletes blank at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'blank arg (interactive-p)))

(defun ar-delete-blank-in-region (beg end)
  "Deletes BLANK at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'blank beg end (interactive-p)))
(defun ar-cntrl-atpt ()
  "Returns cntrl at point if any, nil otherwise. "
  (interactive)
  (ar-th 'cntrl nil nil (interactive-p)))

(defalias 'ar-bounds-of-cntrl-atpt 'ar-cntrl-bounds-atpt)
(defun ar-cntrl-bounds-atpt ()
  "Returns a list, borders of cntrl if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'cntrl nil (interactive-p)))

(defun ar-cntrl-beginning-position-atpt ()
  "Returns a number, beginning position CNTRL at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'cntrl nil (interactive-p)))

(defun ar-cntrl-end-position-atpt ()
  "Returns a number, end position of CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'cntrl nil (interactive-p)))

(defun ar-cntrl-beginning-atpt ()
  "Goto beginning of symbol or char-class CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'cntrl nil (interactive-p)))

(defun ar-cntrl-end-atpt ()
  "Goto end of symbol or char-class CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'cntrl nil (interactive-p)))

(defun ar-in-cntrl-p-atpt ()
  "Returns bounds of CNTRL at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'cntrl nil (interactive-p)))

(defun ar-length-of-cntrl-atpt ()
  "Returns beginning of symbol or char-class CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'cntrl nil (interactive-p)))

(defun ar-copy-cntrl-atpt ()
  "Returns a copy of CNTRL at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'cntrl nil (interactive-p)))

(defun ar-delete-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'cntrl arg (interactive-p)))

(defun ar-forward-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'cntrl arg (interactive-p)))

(defun ar-backward-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'cntrl arg (interactive-p)))

(defun ar-triplequotedq-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'cntrl arg (interactive-p)))

(defun ar-triplequotesq-cntrl-atpt (&optional arg)
  "Deletes cntrl at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'cntrl arg (interactive-p)))

(defun ar-delete-cntrl-in-region (beg end)
  "Deletes CNTRL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'cntrl beg end (interactive-p)))
(defun ar-digit-atpt ()
  "Returns digit at point if any, nil otherwise. "
  (interactive)
  (ar-th 'digit nil nil (interactive-p)))

(defalias 'ar-bounds-of-digit-atpt 'ar-digit-bounds-atpt)
(defun ar-digit-bounds-atpt ()
  "Returns a list, borders of digit if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'digit nil (interactive-p)))

(defun ar-digit-beginning-position-atpt ()
  "Returns a number, beginning position DIGIT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'digit nil (interactive-p)))

(defun ar-digit-end-position-atpt ()
  "Returns a number, end position of DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'digit nil (interactive-p)))

(defun ar-digit-beginning-atpt ()
  "Goto beginning of symbol or char-class DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'digit nil (interactive-p)))

(defun ar-digit-end-atpt ()
  "Goto end of symbol or char-class DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'digit nil (interactive-p)))

(defun ar-in-digit-p-atpt ()
  "Returns bounds of DIGIT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'digit nil (interactive-p)))

(defun ar-length-of-digit-atpt ()
  "Returns beginning of symbol or char-class DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'digit nil (interactive-p)))

(defun ar-copy-digit-atpt ()
  "Returns a copy of DIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'digit nil (interactive-p)))

(defun ar-delete-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'digit arg (interactive-p)))

(defun ar-forward-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'digit arg (interactive-p)))

(defun ar-backward-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'digit arg (interactive-p)))

(defun ar-triplequotedq-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'digit arg (interactive-p)))

(defun ar-triplequotesq-digit-atpt (&optional arg)
  "Deletes digit at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'digit arg (interactive-p)))

(defun ar-delete-digit-in-region (beg end)
  "Deletes DIGIT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'digit beg end (interactive-p)))
(defun ar-graph-atpt ()
  "Returns graph at point if any, nil otherwise. "
  (interactive)
  (ar-th 'graph nil nil (interactive-p)))

(defalias 'ar-bounds-of-graph-atpt 'ar-graph-bounds-atpt)
(defun ar-graph-bounds-atpt ()
  "Returns a list, borders of graph if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'graph nil (interactive-p)))

(defun ar-graph-beginning-position-atpt ()
  "Returns a number, beginning position GRAPH at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'graph nil (interactive-p)))

(defun ar-graph-end-position-atpt ()
  "Returns a number, end position of GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'graph nil (interactive-p)))

(defun ar-graph-beginning-atpt ()
  "Goto beginning of symbol or char-class GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'graph nil (interactive-p)))

(defun ar-graph-end-atpt ()
  "Goto end of symbol or char-class GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'graph nil (interactive-p)))

(defun ar-in-graph-p-atpt ()
  "Returns bounds of GRAPH at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'graph nil (interactive-p)))

(defun ar-length-of-graph-atpt ()
  "Returns beginning of symbol or char-class GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'graph nil (interactive-p)))

(defun ar-copy-graph-atpt ()
  "Returns a copy of GRAPH at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'graph nil (interactive-p)))

(defun ar-delete-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'graph arg (interactive-p)))

(defun ar-forward-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'graph arg (interactive-p)))

(defun ar-backward-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'graph arg (interactive-p)))

(defun ar-triplequotedq-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'graph arg (interactive-p)))

(defun ar-triplequotesq-graph-atpt (&optional arg)
  "Deletes graph at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'graph arg (interactive-p)))

(defun ar-delete-graph-in-region (beg end)
  "Deletes GRAPH at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'graph beg end (interactive-p)))
(defun ar-lower-atpt ()
  "Returns lower at point if any, nil otherwise. "
  (interactive)
  (ar-th 'lower nil nil (interactive-p)))

(defalias 'ar-bounds-of-lower-atpt 'ar-lower-bounds-atpt)
(defun ar-lower-bounds-atpt ()
  "Returns a list, borders of lower if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'lower nil (interactive-p)))

(defun ar-lower-beginning-position-atpt ()
  "Returns a number, beginning position LOWER at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'lower nil (interactive-p)))

(defun ar-lower-end-position-atpt ()
  "Returns a number, end position of LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'lower nil (interactive-p)))

(defun ar-lower-beginning-atpt ()
  "Goto beginning of symbol or char-class LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'lower nil (interactive-p)))

(defun ar-lower-end-atpt ()
  "Goto end of symbol or char-class LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'lower nil (interactive-p)))

(defun ar-in-lower-p-atpt ()
  "Returns bounds of LOWER at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'lower nil (interactive-p)))

(defun ar-length-of-lower-atpt ()
  "Returns beginning of symbol or char-class LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'lower nil (interactive-p)))

(defun ar-copy-lower-atpt ()
  "Returns a copy of LOWER at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'lower nil (interactive-p)))

(defun ar-delete-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'lower arg (interactive-p)))

(defun ar-forward-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'lower arg (interactive-p)))

(defun ar-backward-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'lower arg (interactive-p)))

(defun ar-triplequotedq-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'lower arg (interactive-p)))

(defun ar-triplequotesq-lower-atpt (&optional arg)
  "Deletes lower at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'lower arg (interactive-p)))

(defun ar-delete-lower-in-region (beg end)
  "Deletes LOWER at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'lower beg end (interactive-p)))
(defun ar-nonascii-atpt ()
  "Returns nonascii at point if any, nil otherwise. "
  (interactive)
  (ar-th 'nonascii nil nil (interactive-p)))

(defalias 'ar-bounds-of-nonascii-atpt 'ar-nonascii-bounds-atpt)
(defun ar-nonascii-bounds-atpt ()
  "Returns a list, borders of nonascii if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'nonascii nil (interactive-p)))

(defun ar-nonascii-beginning-position-atpt ()
  "Returns a number, beginning position NONASCII at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'nonascii nil (interactive-p)))

(defun ar-nonascii-end-position-atpt ()
  "Returns a number, end position of NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'nonascii nil (interactive-p)))

(defun ar-nonascii-beginning-atpt ()
  "Goto beginning of symbol or char-class NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'nonascii nil (interactive-p)))

(defun ar-nonascii-end-atpt ()
  "Goto end of symbol or char-class NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'nonascii nil (interactive-p)))

(defun ar-in-nonascii-p-atpt ()
  "Returns bounds of NONASCII at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'nonascii nil (interactive-p)))

(defun ar-length-of-nonascii-atpt ()
  "Returns beginning of symbol or char-class NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'nonascii nil (interactive-p)))

(defun ar-copy-nonascii-atpt ()
  "Returns a copy of NONASCII at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'nonascii nil (interactive-p)))

(defun ar-delete-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'nonascii arg (interactive-p)))

(defun ar-forward-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'nonascii arg (interactive-p)))

(defun ar-backward-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'nonascii arg (interactive-p)))

(defun ar-triplequotedq-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'nonascii arg (interactive-p)))

(defun ar-triplequotesq-nonascii-atpt (&optional arg)
  "Deletes nonascii at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'nonascii arg (interactive-p)))

(defun ar-delete-nonascii-in-region (beg end)
  "Deletes NONASCII at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'nonascii beg end (interactive-p)))
(defun ar-print-atpt ()
  "Returns print at point if any, nil otherwise. "
  (interactive)
  (ar-th 'print nil nil (interactive-p)))

(defalias 'ar-bounds-of-print-atpt 'ar-print-bounds-atpt)
(defun ar-print-bounds-atpt ()
  "Returns a list, borders of print if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'print nil (interactive-p)))

(defun ar-print-beginning-position-atpt ()
  "Returns a number, beginning position PRINT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'print nil (interactive-p)))

(defun ar-print-end-position-atpt ()
  "Returns a number, end position of PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'print nil (interactive-p)))

(defun ar-print-beginning-atpt ()
  "Goto beginning of symbol or char-class PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'print nil (interactive-p)))

(defun ar-print-end-atpt ()
  "Goto end of symbol or char-class PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'print nil (interactive-p)))

(defun ar-in-print-p-atpt ()
  "Returns bounds of PRINT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'print nil (interactive-p)))

(defun ar-length-of-print-atpt ()
  "Returns beginning of symbol or char-class PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'print nil (interactive-p)))

(defun ar-copy-print-atpt ()
  "Returns a copy of PRINT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'print nil (interactive-p)))

(defun ar-delete-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'print arg (interactive-p)))

(defun ar-forward-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'print arg (interactive-p)))

(defun ar-backward-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'print arg (interactive-p)))

(defun ar-triplequotedq-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'print arg (interactive-p)))

(defun ar-triplequotesq-print-atpt (&optional arg)
  "Deletes print at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'print arg (interactive-p)))

(defun ar-delete-print-in-region (beg end)
  "Deletes PRINT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'print beg end (interactive-p)))
(defun ar-punct-atpt ()
  "Returns punct at point if any, nil otherwise. "
  (interactive)
  (ar-th 'punct nil nil (interactive-p)))

(defalias 'ar-bounds-of-punct-atpt 'ar-punct-bounds-atpt)
(defun ar-punct-bounds-atpt ()
  "Returns a list, borders of punct if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'punct nil (interactive-p)))

(defun ar-punct-beginning-position-atpt ()
  "Returns a number, beginning position PUNCT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'punct nil (interactive-p)))

(defun ar-punct-end-position-atpt ()
  "Returns a number, end position of PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'punct nil (interactive-p)))

(defun ar-punct-beginning-atpt ()
  "Goto beginning of symbol or char-class PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'punct nil (interactive-p)))

(defun ar-punct-end-atpt ()
  "Goto end of symbol or char-class PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'punct nil (interactive-p)))

(defun ar-in-punct-p-atpt ()
  "Returns bounds of PUNCT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'punct nil (interactive-p)))

(defun ar-length-of-punct-atpt ()
  "Returns beginning of symbol or char-class PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'punct nil (interactive-p)))

(defun ar-copy-punct-atpt ()
  "Returns a copy of PUNCT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'punct nil (interactive-p)))

(defun ar-delete-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'punct arg (interactive-p)))

(defun ar-forward-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'punct arg (interactive-p)))

(defun ar-backward-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'punct arg (interactive-p)))

(defun ar-triplequotedq-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'punct arg (interactive-p)))

(defun ar-triplequotesq-punct-atpt (&optional arg)
  "Deletes punct at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'punct arg (interactive-p)))

(defun ar-delete-punct-in-region (beg end)
  "Deletes PUNCT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'punct beg end (interactive-p)))
(defun ar-space-atpt ()
  "Returns space at point if any, nil otherwise. "
  (interactive)
  (ar-th 'space nil nil (interactive-p)))

(defalias 'ar-bounds-of-space-atpt 'ar-space-bounds-atpt)
(defun ar-space-bounds-atpt ()
  "Returns a list, borders of space if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'space nil (interactive-p)))

(defun ar-space-beginning-position-atpt ()
  "Returns a number, beginning position SPACE at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'space nil (interactive-p)))

(defun ar-space-end-position-atpt ()
  "Returns a number, end position of SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'space nil (interactive-p)))

(defun ar-space-beginning-atpt ()
  "Goto beginning of symbol or char-class SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'space nil (interactive-p)))

(defun ar-space-end-atpt ()
  "Goto end of symbol or char-class SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'space nil (interactive-p)))

(defun ar-in-space-p-atpt ()
  "Returns bounds of SPACE at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'space nil (interactive-p)))

(defun ar-length-of-space-atpt ()
  "Returns beginning of symbol or char-class SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'space nil (interactive-p)))

(defun ar-copy-space-atpt ()
  "Returns a copy of SPACE at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'space nil (interactive-p)))

(defun ar-delete-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'space arg (interactive-p)))

(defun ar-forward-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'space arg (interactive-p)))

(defun ar-backward-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'space arg (interactive-p)))

(defun ar-triplequotedq-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'space arg (interactive-p)))

(defun ar-triplequotesq-space-atpt (&optional arg)
  "Deletes space at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'space arg (interactive-p)))

(defun ar-delete-space-in-region (beg end)
  "Deletes SPACE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'space beg end (interactive-p)))
(defun ar-upper-atpt ()
  "Returns upper at point if any, nil otherwise. "
  (interactive)
  (ar-th 'upper nil nil (interactive-p)))

(defalias 'ar-bounds-of-upper-atpt 'ar-upper-bounds-atpt)
(defun ar-upper-bounds-atpt ()
  "Returns a list, borders of upper if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'upper nil (interactive-p)))

(defun ar-upper-beginning-position-atpt ()
  "Returns a number, beginning position UPPER at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'upper nil (interactive-p)))

(defun ar-upper-end-position-atpt ()
  "Returns a number, end position of UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'upper nil (interactive-p)))

(defun ar-upper-beginning-atpt ()
  "Goto beginning of symbol or char-class UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'upper nil (interactive-p)))

(defun ar-upper-end-atpt ()
  "Goto end of symbol or char-class UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'upper nil (interactive-p)))

(defun ar-in-upper-p-atpt ()
  "Returns bounds of UPPER at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'upper nil (interactive-p)))

(defun ar-length-of-upper-atpt ()
  "Returns beginning of symbol or char-class UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'upper nil (interactive-p)))

(defun ar-copy-upper-atpt ()
  "Returns a copy of UPPER at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'upper nil (interactive-p)))

(defun ar-delete-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'upper arg (interactive-p)))

(defun ar-forward-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'upper arg (interactive-p)))

(defun ar-backward-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'upper arg (interactive-p)))

(defun ar-triplequotedq-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'upper arg (interactive-p)))

(defun ar-triplequotesq-upper-atpt (&optional arg)
  "Deletes upper at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'upper arg (interactive-p)))

(defun ar-delete-upper-in-region (beg end)
  "Deletes UPPER at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'upper beg end (interactive-p)))
(defun ar-xdigit-atpt ()
  "Returns xdigit at point if any, nil otherwise. "
  (interactive)
  (ar-th 'xdigit nil nil (interactive-p)))

(defalias 'ar-bounds-of-xdigit-atpt 'ar-xdigit-bounds-atpt)
(defun ar-xdigit-bounds-atpt ()
  "Returns a list, borders of xdigit if any, nil otherwise. "
  (interactive)
  (ar-th-bounds 'xdigit nil (interactive-p)))

(defun ar-xdigit-beginning-position-atpt ()
  "Returns a number, beginning position XDIGIT at point if any, nil otherwise.  "
  (interactive)
  (ar-th-beg 'xdigit nil (interactive-p)))

(defun ar-xdigit-end-position-atpt ()
  "Returns a number, end position of XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-end 'xdigit nil (interactive-p)))

(defun ar-xdigit-beginning-atpt ()
  "Goto beginning of symbol or char-class XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotobeg 'xdigit nil (interactive-p)))

(defun ar-xdigit-end-atpt ()
  "Goto end of symbol or char-class XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-gotoend 'xdigit nil (interactive-p)))

(defun ar-in-xdigit-p-atpt ()
  "Returns bounds of XDIGIT at point, a list, if inside, nil otherwise. "
  (interactive)
  (ar-th-bounds 'xdigit nil (interactive-p)))

(defun ar-length-of-xdigit-atpt ()
  "Returns beginning of symbol or char-class XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-length 'xdigit nil (interactive-p)))

(defun ar-copy-xdigit-atpt ()
  "Returns a copy of XDIGIT at point if any, nil otherwise. "
  (interactive)
  (ar-th-copy 'xdigit nil (interactive-p)))

(defun ar-delete-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'xdigit arg (interactive-p)))

(defun ar-forward-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-forward 'xdigit arg (interactive-p)))

(defun ar-backward-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-backward 'xdigit arg (interactive-p)))

(defun ar-triplequotedq-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotedq 'xdigit arg (interactive-p)))

(defun ar-triplequotesq-xdigit-atpt (&optional arg)
  "Deletes xdigit at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-triplequotesq 'xdigit arg (interactive-p)))

(defun ar-delete-xdigit-in-region (beg end)
  "Deletes XDIGIT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'xdigit beg end (interactive-p)));; ar-thing-at-point-utils-nodelim-core ar-atpt-classes: end

;; ar-thing-at-point-utils-unpaired-delim-aktiv ar-atpt-classes ar-unpaired-delimlist-aktiv: start


(defun ar-backslash-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'alnum nil))

(defun ar-backtick-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'alnum nil))

(defun ar-colon-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'alnum nil))

(defun ar-cross-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'alnum nil))

(defun ar-dollar-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'alnum nil))

(defun ar-doublequote-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'alnum nil))

(defun ar-equalize-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'alnum nil))

(defun ar-escape-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'alnum nil))

(defun ar-hash-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'alnum nil))

(defun ar-hyphen-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'alnum nil))

(defun ar-singlequote-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'alnum nil))

(defun ar-slash-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'alnum nil))

(defun ar-star-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'alnum nil))

(defun ar-tild-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'alnum nil))

(defun ar-underscore-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'alnum nil))

(defun ar-whitespace-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'alnum nil))

(defun ar-doubleslash-alnum-atpt ()
  "ALNUM ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'alnum nil))

(defun ar-backslash-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'alpha nil))

(defun ar-backtick-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'alpha nil))

(defun ar-colon-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'alpha nil))

(defun ar-cross-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'alpha nil))

(defun ar-dollar-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'alpha nil))

(defun ar-doublequote-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'alpha nil))

(defun ar-equalize-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'alpha nil))

(defun ar-escape-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'alpha nil))

(defun ar-hash-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'alpha nil))

(defun ar-hyphen-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'alpha nil))

(defun ar-singlequote-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'alpha nil))

(defun ar-slash-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'alpha nil))

(defun ar-star-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'alpha nil))

(defun ar-tild-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'alpha nil))

(defun ar-underscore-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'alpha nil))

(defun ar-whitespace-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'alpha nil))

(defun ar-doubleslash-alpha-atpt ()
  "ALPHA ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'alpha nil))

(defun ar-backslash-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'ascii nil))

(defun ar-backtick-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'ascii nil))

(defun ar-colon-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'ascii nil))

(defun ar-cross-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'ascii nil))

(defun ar-dollar-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'ascii nil))

(defun ar-doublequote-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'ascii nil))

(defun ar-equalize-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'ascii nil))

(defun ar-escape-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'ascii nil))

(defun ar-hash-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'ascii nil))

(defun ar-hyphen-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'ascii nil))

(defun ar-singlequote-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'ascii nil))

(defun ar-slash-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'ascii nil))

(defun ar-star-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'ascii nil))

(defun ar-tild-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'ascii nil))

(defun ar-underscore-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'ascii nil))

(defun ar-whitespace-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'ascii nil))

(defun ar-doubleslash-ascii-atpt ()
  "ASCII ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'ascii nil))

(defun ar-backslash-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'blank nil))

(defun ar-backtick-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'blank nil))

(defun ar-colon-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'blank nil))

(defun ar-cross-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'blank nil))

(defun ar-dollar-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'blank nil))

(defun ar-doublequote-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'blank nil))

(defun ar-equalize-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'blank nil))

(defun ar-escape-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'blank nil))

(defun ar-hash-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'blank nil))

(defun ar-hyphen-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'blank nil))

(defun ar-singlequote-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'blank nil))

(defun ar-slash-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'blank nil))

(defun ar-star-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'blank nil))

(defun ar-tild-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'blank nil))

(defun ar-underscore-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'blank nil))

(defun ar-whitespace-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'blank nil))

(defun ar-doubleslash-blank-atpt ()
  "BLANK BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'blank nil))

(defun ar-backslash-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'cntrl nil))

(defun ar-backtick-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'cntrl nil))

(defun ar-colon-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'cntrl nil))

(defun ar-cross-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'cntrl nil))

(defun ar-dollar-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'cntrl nil))

(defun ar-doublequote-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'cntrl nil))

(defun ar-equalize-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'cntrl nil))

(defun ar-escape-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'cntrl nil))

(defun ar-hash-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'cntrl nil))

(defun ar-hyphen-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'cntrl nil))

(defun ar-singlequote-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'cntrl nil))

(defun ar-slash-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'cntrl nil))

(defun ar-star-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'cntrl nil))

(defun ar-tild-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'cntrl nil))

(defun ar-underscore-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'cntrl nil))

(defun ar-whitespace-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'cntrl nil))

(defun ar-doubleslash-cntrl-atpt ()
  "CNTRL CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'cntrl nil))

(defun ar-backslash-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'digit nil))

(defun ar-backtick-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'digit nil))

(defun ar-colon-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'digit nil))

(defun ar-cross-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'digit nil))

(defun ar-dollar-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'digit nil))

(defun ar-doublequote-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'digit nil))

(defun ar-equalize-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'digit nil))

(defun ar-escape-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'digit nil))

(defun ar-hash-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'digit nil))

(defun ar-hyphen-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'digit nil))

(defun ar-singlequote-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'digit nil))

(defun ar-slash-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'digit nil))

(defun ar-star-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'digit nil))

(defun ar-tild-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'digit nil))

(defun ar-underscore-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'digit nil))

(defun ar-whitespace-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'digit nil))

(defun ar-doubleslash-digit-atpt ()
  "DIGIT DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'digit nil))

(defun ar-backslash-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'graph nil))

(defun ar-backtick-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'graph nil))

(defun ar-colon-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'graph nil))

(defun ar-cross-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'graph nil))

(defun ar-dollar-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'graph nil))

(defun ar-doublequote-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'graph nil))

(defun ar-equalize-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'graph nil))

(defun ar-escape-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'graph nil))

(defun ar-hash-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'graph nil))

(defun ar-hyphen-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'graph nil))

(defun ar-singlequote-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'graph nil))

(defun ar-slash-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'graph nil))

(defun ar-star-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'graph nil))

(defun ar-tild-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'graph nil))

(defun ar-underscore-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'graph nil))

(defun ar-whitespace-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'graph nil))

(defun ar-doubleslash-graph-atpt ()
  "GRAPH GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'graph nil))

(defun ar-backslash-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'lower nil))

(defun ar-backtick-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'lower nil))

(defun ar-colon-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'lower nil))

(defun ar-cross-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'lower nil))

(defun ar-dollar-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'lower nil))

(defun ar-doublequote-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'lower nil))

(defun ar-equalize-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'lower nil))

(defun ar-escape-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'lower nil))

(defun ar-hash-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'lower nil))

(defun ar-hyphen-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'lower nil))

(defun ar-singlequote-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'lower nil))

(defun ar-slash-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'lower nil))

(defun ar-star-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'lower nil))

(defun ar-tild-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'lower nil))

(defun ar-underscore-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'lower nil))

(defun ar-whitespace-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'lower nil))

(defun ar-doubleslash-lower-atpt ()
  "LOWER LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'lower nil))

(defun ar-backslash-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'nonascii nil))

(defun ar-backtick-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'nonascii nil))

(defun ar-colon-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'nonascii nil))

(defun ar-cross-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'nonascii nil))

(defun ar-dollar-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'nonascii nil))

(defun ar-doublequote-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'nonascii nil))

(defun ar-equalize-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'nonascii nil))

(defun ar-escape-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'nonascii nil))

(defun ar-hash-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'nonascii nil))

(defun ar-hyphen-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'nonascii nil))

(defun ar-singlequote-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'nonascii nil))

(defun ar-slash-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'nonascii nil))

(defun ar-star-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'nonascii nil))

(defun ar-tild-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'nonascii nil))

(defun ar-underscore-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'nonascii nil))

(defun ar-whitespace-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'nonascii nil))

(defun ar-doubleslash-nonascii-atpt ()
  "NONASCII NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'nonascii nil))

(defun ar-backslash-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'print nil))

(defun ar-backtick-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'print nil))

(defun ar-colon-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'print nil))

(defun ar-cross-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'print nil))

(defun ar-dollar-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'print nil))

(defun ar-doublequote-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'print nil))

(defun ar-equalize-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'print nil))

(defun ar-escape-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'print nil))

(defun ar-hash-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'print nil))

(defun ar-hyphen-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'print nil))

(defun ar-singlequote-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'print nil))

(defun ar-slash-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'print nil))

(defun ar-star-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'print nil))

(defun ar-tild-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'print nil))

(defun ar-underscore-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'print nil))

(defun ar-whitespace-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'print nil))

(defun ar-doubleslash-print-atpt ()
  "PRINT PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'print nil))

(defun ar-backslash-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'punct nil))

(defun ar-backtick-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'punct nil))

(defun ar-colon-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'punct nil))

(defun ar-cross-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'punct nil))

(defun ar-dollar-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'punct nil))

(defun ar-doublequote-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'punct nil))

(defun ar-equalize-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'punct nil))

(defun ar-escape-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'punct nil))

(defun ar-hash-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'punct nil))

(defun ar-hyphen-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'punct nil))

(defun ar-singlequote-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'punct nil))

(defun ar-slash-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'punct nil))

(defun ar-star-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'punct nil))

(defun ar-tild-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'punct nil))

(defun ar-underscore-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'punct nil))

(defun ar-whitespace-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'punct nil))

(defun ar-doubleslash-punct-atpt ()
  "PUNCT PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'punct nil))

(defun ar-backslash-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'space nil))

(defun ar-backtick-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'space nil))

(defun ar-colon-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'space nil))

(defun ar-cross-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'space nil))

(defun ar-dollar-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'space nil))

(defun ar-doublequote-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'space nil))

(defun ar-equalize-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'space nil))

(defun ar-escape-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'space nil))

(defun ar-hash-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'space nil))

(defun ar-hyphen-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'space nil))

(defun ar-singlequote-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'space nil))

(defun ar-slash-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'space nil))

(defun ar-star-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'space nil))

(defun ar-tild-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'space nil))

(defun ar-underscore-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'space nil))

(defun ar-whitespace-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'space nil))

(defun ar-doubleslash-space-atpt ()
  "SPACE SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'space nil))

(defun ar-backslash-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'upper nil))

(defun ar-backtick-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'upper nil))

(defun ar-colon-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'upper nil))

(defun ar-cross-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'upper nil))

(defun ar-dollar-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'upper nil))

(defun ar-doublequote-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'upper nil))

(defun ar-equalize-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'upper nil))

(defun ar-escape-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'upper nil))

(defun ar-hash-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'upper nil))

(defun ar-hyphen-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'upper nil))

(defun ar-singlequote-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'upper nil))

(defun ar-slash-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'upper nil))

(defun ar-star-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'upper nil))

(defun ar-tild-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'upper nil))

(defun ar-underscore-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'upper nil))

(defun ar-whitespace-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'upper nil))

(defun ar-doubleslash-upper-atpt ()
  "UPPER UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'upper nil))

(defun ar-backslash-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backslash 'xdigit nil))

(defun ar-backtick-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-backtick 'xdigit nil))

(defun ar-colon-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-colon 'xdigit nil))

(defun ar-cross-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-cross 'xdigit nil))

(defun ar-dollar-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-dollar 'xdigit nil))

(defun ar-doublequote-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doublequote 'xdigit nil))

(defun ar-equalize-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-equalize 'xdigit nil))

(defun ar-escape-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-escape 'xdigit nil))

(defun ar-hash-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hash 'xdigit nil))

(defun ar-hyphen-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-hyphen 'xdigit nil))

(defun ar-singlequote-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-singlequote 'xdigit nil))

(defun ar-slash-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-slash 'xdigit nil))

(defun ar-star-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-star 'xdigit nil))

(defun ar-tild-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-tild 'xdigit nil))

(defun ar-underscore-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-underscore 'xdigit nil))

(defun ar-whitespace-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-whitespace 'xdigit nil))

(defun ar-doubleslash-xdigit-atpt ()
  "XDIGIT XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-doubleslash 'xdigit nil));; ar-thing-at-point-utils-unpaired-delim-aktiv ar-atpt-classes ar-unpaired-delimlist-aktiv: end

;; ar-thing-at-point-utils-delim-classes-paired: start


(defalias 'ar-brace-alnum-atpt 'ar-alnum-brace-atpt)
(defun ar-alnum-brace-atpt ()
  "Brace ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'alnum nil))

(defalias 'ar-bracket-alnum-atpt 'ar-alnum-bracket-atpt)
(defun ar-alnum-bracket-atpt ()
  "Bracket ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'alnum nil))

(defalias 'ar-lesserangle-alnum-atpt 'ar-alnum-lesserangle-atpt)
(defun ar-alnum-lesserangle-atpt ()
  "Lesserangle ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'alnum nil))

(defalias 'ar-greaterangle-alnum-atpt 'ar-alnum-greaterangle-atpt)
(defun ar-alnum-greaterangle-atpt ()
  "Greaterangle ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'alnum nil))

(defalias 'ar-leftrightsinglequote-alnum-atpt 'ar-alnum-leftrightsinglequote-atpt)
(defun ar-alnum-leftrightsinglequote-atpt ()
  "Leftrightsinglequote ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'alnum nil))

(defalias 'ar-leftrightsinglequote-alnum-atpt 'ar-alnum-leftrightsinglequote-atpt)
(defun ar-alnum-leftrightsinglequote-atpt ()
  "Leftrightsinglequote ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'alnum nil))

(defalias 'ar-parentize-alnum-atpt 'ar-alnum-parentize-atpt)
(defun ar-alnum-parentize-atpt ()
  "Parentize ALNUM at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'alnum nil))

(defalias 'ar-brace-alpha-atpt 'ar-alpha-brace-atpt)
(defun ar-alpha-brace-atpt ()
  "Brace ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'alpha nil))

(defalias 'ar-bracket-alpha-atpt 'ar-alpha-bracket-atpt)
(defun ar-alpha-bracket-atpt ()
  "Bracket ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'alpha nil))

(defalias 'ar-lesserangle-alpha-atpt 'ar-alpha-lesserangle-atpt)
(defun ar-alpha-lesserangle-atpt ()
  "Lesserangle ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'alpha nil))

(defalias 'ar-greaterangle-alpha-atpt 'ar-alpha-greaterangle-atpt)
(defun ar-alpha-greaterangle-atpt ()
  "Greaterangle ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'alpha nil))

(defalias 'ar-leftrightsinglequote-alpha-atpt 'ar-alpha-leftrightsinglequote-atpt)
(defun ar-alpha-leftrightsinglequote-atpt ()
  "Leftrightsinglequote ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'alpha nil))

(defalias 'ar-leftrightsinglequote-alpha-atpt 'ar-alpha-leftrightsinglequote-atpt)
(defun ar-alpha-leftrightsinglequote-atpt ()
  "Leftrightsinglequote ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'alpha nil))

(defalias 'ar-parentize-alpha-atpt 'ar-alpha-parentize-atpt)
(defun ar-alpha-parentize-atpt ()
  "Parentize ALPHA at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'alpha nil))

(defalias 'ar-brace-ascii-atpt 'ar-ascii-brace-atpt)
(defun ar-ascii-brace-atpt ()
  "Brace ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'ascii nil))

(defalias 'ar-bracket-ascii-atpt 'ar-ascii-bracket-atpt)
(defun ar-ascii-bracket-atpt ()
  "Bracket ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'ascii nil))

(defalias 'ar-lesserangle-ascii-atpt 'ar-ascii-lesserangle-atpt)
(defun ar-ascii-lesserangle-atpt ()
  "Lesserangle ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'ascii nil))

(defalias 'ar-greaterangle-ascii-atpt 'ar-ascii-greaterangle-atpt)
(defun ar-ascii-greaterangle-atpt ()
  "Greaterangle ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'ascii nil))

(defalias 'ar-leftrightsinglequote-ascii-atpt 'ar-ascii-leftrightsinglequote-atpt)
(defun ar-ascii-leftrightsinglequote-atpt ()
  "Leftrightsinglequote ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'ascii nil))

(defalias 'ar-leftrightsinglequote-ascii-atpt 'ar-ascii-leftrightsinglequote-atpt)
(defun ar-ascii-leftrightsinglequote-atpt ()
  "Leftrightsinglequote ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'ascii nil))

(defalias 'ar-parentize-ascii-atpt 'ar-ascii-parentize-atpt)
(defun ar-ascii-parentize-atpt ()
  "Parentize ASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'ascii nil))

(defalias 'ar-brace-blank-atpt 'ar-blank-brace-atpt)
(defun ar-blank-brace-atpt ()
  "Brace BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'blank nil))

(defalias 'ar-bracket-blank-atpt 'ar-blank-bracket-atpt)
(defun ar-blank-bracket-atpt ()
  "Bracket BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'blank nil))

(defalias 'ar-lesserangle-blank-atpt 'ar-blank-lesserangle-atpt)
(defun ar-blank-lesserangle-atpt ()
  "Lesserangle BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'blank nil))

(defalias 'ar-greaterangle-blank-atpt 'ar-blank-greaterangle-atpt)
(defun ar-blank-greaterangle-atpt ()
  "Greaterangle BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'blank nil))

(defalias 'ar-leftrightsinglequote-blank-atpt 'ar-blank-leftrightsinglequote-atpt)
(defun ar-blank-leftrightsinglequote-atpt ()
  "Leftrightsinglequote BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'blank nil))

(defalias 'ar-leftrightsinglequote-blank-atpt 'ar-blank-leftrightsinglequote-atpt)
(defun ar-blank-leftrightsinglequote-atpt ()
  "Leftrightsinglequote BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'blank nil))

(defalias 'ar-parentize-blank-atpt 'ar-blank-parentize-atpt)
(defun ar-blank-parentize-atpt ()
  "Parentize BLANK at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'blank nil))

(defalias 'ar-brace-cntrl-atpt 'ar-cntrl-brace-atpt)
(defun ar-cntrl-brace-atpt ()
  "Brace CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'cntrl nil))

(defalias 'ar-bracket-cntrl-atpt 'ar-cntrl-bracket-atpt)
(defun ar-cntrl-bracket-atpt ()
  "Bracket CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'cntrl nil))

(defalias 'ar-lesserangle-cntrl-atpt 'ar-cntrl-lesserangle-atpt)
(defun ar-cntrl-lesserangle-atpt ()
  "Lesserangle CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'cntrl nil))

(defalias 'ar-greaterangle-cntrl-atpt 'ar-cntrl-greaterangle-atpt)
(defun ar-cntrl-greaterangle-atpt ()
  "Greaterangle CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'cntrl nil))

(defalias 'ar-leftrightsinglequote-cntrl-atpt 'ar-cntrl-leftrightsinglequote-atpt)
(defun ar-cntrl-leftrightsinglequote-atpt ()
  "Leftrightsinglequote CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'cntrl nil))

(defalias 'ar-leftrightsinglequote-cntrl-atpt 'ar-cntrl-leftrightsinglequote-atpt)
(defun ar-cntrl-leftrightsinglequote-atpt ()
  "Leftrightsinglequote CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'cntrl nil))

(defalias 'ar-parentize-cntrl-atpt 'ar-cntrl-parentize-atpt)
(defun ar-cntrl-parentize-atpt ()
  "Parentize CNTRL at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'cntrl nil))

(defalias 'ar-brace-digit-atpt 'ar-digit-brace-atpt)
(defun ar-digit-brace-atpt ()
  "Brace DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'digit nil))

(defalias 'ar-bracket-digit-atpt 'ar-digit-bracket-atpt)
(defun ar-digit-bracket-atpt ()
  "Bracket DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'digit nil))

(defalias 'ar-lesserangle-digit-atpt 'ar-digit-lesserangle-atpt)
(defun ar-digit-lesserangle-atpt ()
  "Lesserangle DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'digit nil))

(defalias 'ar-greaterangle-digit-atpt 'ar-digit-greaterangle-atpt)
(defun ar-digit-greaterangle-atpt ()
  "Greaterangle DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'digit nil))

(defalias 'ar-leftrightsinglequote-digit-atpt 'ar-digit-leftrightsinglequote-atpt)
(defun ar-digit-leftrightsinglequote-atpt ()
  "Leftrightsinglequote DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'digit nil))

(defalias 'ar-leftrightsinglequote-digit-atpt 'ar-digit-leftrightsinglequote-atpt)
(defun ar-digit-leftrightsinglequote-atpt ()
  "Leftrightsinglequote DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'digit nil))

(defalias 'ar-parentize-digit-atpt 'ar-digit-parentize-atpt)
(defun ar-digit-parentize-atpt ()
  "Parentize DIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'digit nil))

(defalias 'ar-brace-graph-atpt 'ar-graph-brace-atpt)
(defun ar-graph-brace-atpt ()
  "Brace GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'graph nil))

(defalias 'ar-bracket-graph-atpt 'ar-graph-bracket-atpt)
(defun ar-graph-bracket-atpt ()
  "Bracket GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'graph nil))

(defalias 'ar-lesserangle-graph-atpt 'ar-graph-lesserangle-atpt)
(defun ar-graph-lesserangle-atpt ()
  "Lesserangle GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'graph nil))

(defalias 'ar-greaterangle-graph-atpt 'ar-graph-greaterangle-atpt)
(defun ar-graph-greaterangle-atpt ()
  "Greaterangle GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'graph nil))

(defalias 'ar-leftrightsinglequote-graph-atpt 'ar-graph-leftrightsinglequote-atpt)
(defun ar-graph-leftrightsinglequote-atpt ()
  "Leftrightsinglequote GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'graph nil))

(defalias 'ar-leftrightsinglequote-graph-atpt 'ar-graph-leftrightsinglequote-atpt)
(defun ar-graph-leftrightsinglequote-atpt ()
  "Leftrightsinglequote GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'graph nil))

(defalias 'ar-parentize-graph-atpt 'ar-graph-parentize-atpt)
(defun ar-graph-parentize-atpt ()
  "Parentize GRAPH at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'graph nil))

(defalias 'ar-brace-lower-atpt 'ar-lower-brace-atpt)
(defun ar-lower-brace-atpt ()
  "Brace LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'lower nil))

(defalias 'ar-bracket-lower-atpt 'ar-lower-bracket-atpt)
(defun ar-lower-bracket-atpt ()
  "Bracket LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'lower nil))

(defalias 'ar-lesserangle-lower-atpt 'ar-lower-lesserangle-atpt)
(defun ar-lower-lesserangle-atpt ()
  "Lesserangle LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'lower nil))

(defalias 'ar-greaterangle-lower-atpt 'ar-lower-greaterangle-atpt)
(defun ar-lower-greaterangle-atpt ()
  "Greaterangle LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'lower nil))

(defalias 'ar-leftrightsinglequote-lower-atpt 'ar-lower-leftrightsinglequote-atpt)
(defun ar-lower-leftrightsinglequote-atpt ()
  "Leftrightsinglequote LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'lower nil))

(defalias 'ar-leftrightsinglequote-lower-atpt 'ar-lower-leftrightsinglequote-atpt)
(defun ar-lower-leftrightsinglequote-atpt ()
  "Leftrightsinglequote LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'lower nil))

(defalias 'ar-parentize-lower-atpt 'ar-lower-parentize-atpt)
(defun ar-lower-parentize-atpt ()
  "Parentize LOWER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'lower nil))

(defalias 'ar-brace-nonascii-atpt 'ar-nonascii-brace-atpt)
(defun ar-nonascii-brace-atpt ()
  "Brace NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'nonascii nil))

(defalias 'ar-bracket-nonascii-atpt 'ar-nonascii-bracket-atpt)
(defun ar-nonascii-bracket-atpt ()
  "Bracket NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'nonascii nil))

(defalias 'ar-lesserangle-nonascii-atpt 'ar-nonascii-lesserangle-atpt)
(defun ar-nonascii-lesserangle-atpt ()
  "Lesserangle NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'nonascii nil))

(defalias 'ar-greaterangle-nonascii-atpt 'ar-nonascii-greaterangle-atpt)
(defun ar-nonascii-greaterangle-atpt ()
  "Greaterangle NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'nonascii nil))

(defalias 'ar-leftrightsinglequote-nonascii-atpt 'ar-nonascii-leftrightsinglequote-atpt)
(defun ar-nonascii-leftrightsinglequote-atpt ()
  "Leftrightsinglequote NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'nonascii nil))

(defalias 'ar-leftrightsinglequote-nonascii-atpt 'ar-nonascii-leftrightsinglequote-atpt)
(defun ar-nonascii-leftrightsinglequote-atpt ()
  "Leftrightsinglequote NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'nonascii nil))

(defalias 'ar-parentize-nonascii-atpt 'ar-nonascii-parentize-atpt)
(defun ar-nonascii-parentize-atpt ()
  "Parentize NONASCII at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'nonascii nil))

(defalias 'ar-brace-print-atpt 'ar-print-brace-atpt)
(defun ar-print-brace-atpt ()
  "Brace PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'print nil))

(defalias 'ar-bracket-print-atpt 'ar-print-bracket-atpt)
(defun ar-print-bracket-atpt ()
  "Bracket PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'print nil))

(defalias 'ar-lesserangle-print-atpt 'ar-print-lesserangle-atpt)
(defun ar-print-lesserangle-atpt ()
  "Lesserangle PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'print nil))

(defalias 'ar-greaterangle-print-atpt 'ar-print-greaterangle-atpt)
(defun ar-print-greaterangle-atpt ()
  "Greaterangle PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'print nil))

(defalias 'ar-leftrightsinglequote-print-atpt 'ar-print-leftrightsinglequote-atpt)
(defun ar-print-leftrightsinglequote-atpt ()
  "Leftrightsinglequote PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'print nil))

(defalias 'ar-leftrightsinglequote-print-atpt 'ar-print-leftrightsinglequote-atpt)
(defun ar-print-leftrightsinglequote-atpt ()
  "Leftrightsinglequote PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'print nil))

(defalias 'ar-parentize-print-atpt 'ar-print-parentize-atpt)
(defun ar-print-parentize-atpt ()
  "Parentize PRINT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'print nil))

(defalias 'ar-brace-punct-atpt 'ar-punct-brace-atpt)
(defun ar-punct-brace-atpt ()
  "Brace PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'punct nil))

(defalias 'ar-bracket-punct-atpt 'ar-punct-bracket-atpt)
(defun ar-punct-bracket-atpt ()
  "Bracket PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'punct nil))

(defalias 'ar-lesserangle-punct-atpt 'ar-punct-lesserangle-atpt)
(defun ar-punct-lesserangle-atpt ()
  "Lesserangle PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'punct nil))

(defalias 'ar-greaterangle-punct-atpt 'ar-punct-greaterangle-atpt)
(defun ar-punct-greaterangle-atpt ()
  "Greaterangle PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'punct nil))

(defalias 'ar-leftrightsinglequote-punct-atpt 'ar-punct-leftrightsinglequote-atpt)
(defun ar-punct-leftrightsinglequote-atpt ()
  "Leftrightsinglequote PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'punct nil))

(defalias 'ar-leftrightsinglequote-punct-atpt 'ar-punct-leftrightsinglequote-atpt)
(defun ar-punct-leftrightsinglequote-atpt ()
  "Leftrightsinglequote PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'punct nil))

(defalias 'ar-parentize-punct-atpt 'ar-punct-parentize-atpt)
(defun ar-punct-parentize-atpt ()
  "Parentize PUNCT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'punct nil))

(defalias 'ar-brace-space-atpt 'ar-space-brace-atpt)
(defun ar-space-brace-atpt ()
  "Brace SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'space nil))

(defalias 'ar-bracket-space-atpt 'ar-space-bracket-atpt)
(defun ar-space-bracket-atpt ()
  "Bracket SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'space nil))

(defalias 'ar-lesserangle-space-atpt 'ar-space-lesserangle-atpt)
(defun ar-space-lesserangle-atpt ()
  "Lesserangle SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'space nil))

(defalias 'ar-greaterangle-space-atpt 'ar-space-greaterangle-atpt)
(defun ar-space-greaterangle-atpt ()
  "Greaterangle SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'space nil))

(defalias 'ar-leftrightsinglequote-space-atpt 'ar-space-leftrightsinglequote-atpt)
(defun ar-space-leftrightsinglequote-atpt ()
  "Leftrightsinglequote SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'space nil))

(defalias 'ar-leftrightsinglequote-space-atpt 'ar-space-leftrightsinglequote-atpt)
(defun ar-space-leftrightsinglequote-atpt ()
  "Leftrightsinglequote SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'space nil))

(defalias 'ar-parentize-space-atpt 'ar-space-parentize-atpt)
(defun ar-space-parentize-atpt ()
  "Parentize SPACE at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'space nil))

(defalias 'ar-brace-upper-atpt 'ar-upper-brace-atpt)
(defun ar-upper-brace-atpt ()
  "Brace UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'upper nil))

(defalias 'ar-bracket-upper-atpt 'ar-upper-bracket-atpt)
(defun ar-upper-bracket-atpt ()
  "Bracket UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'upper nil))

(defalias 'ar-lesserangle-upper-atpt 'ar-upper-lesserangle-atpt)
(defun ar-upper-lesserangle-atpt ()
  "Lesserangle UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'upper nil))

(defalias 'ar-greaterangle-upper-atpt 'ar-upper-greaterangle-atpt)
(defun ar-upper-greaterangle-atpt ()
  "Greaterangle UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'upper nil))

(defalias 'ar-leftrightsinglequote-upper-atpt 'ar-upper-leftrightsinglequote-atpt)
(defun ar-upper-leftrightsinglequote-atpt ()
  "Leftrightsinglequote UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'upper nil))

(defalias 'ar-leftrightsinglequote-upper-atpt 'ar-upper-leftrightsinglequote-atpt)
(defun ar-upper-leftrightsinglequote-atpt ()
  "Leftrightsinglequote UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'upper nil))

(defalias 'ar-parentize-upper-atpt 'ar-upper-parentize-atpt)
(defun ar-upper-parentize-atpt ()
  "Parentize UPPER at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'upper nil))

(defalias 'ar-brace-xdigit-atpt 'ar-xdigit-brace-atpt)
(defun ar-xdigit-brace-atpt ()
  "Brace XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-brace 'xdigit nil))

(defalias 'ar-bracket-xdigit-atpt 'ar-xdigit-bracket-atpt)
(defun ar-xdigit-bracket-atpt ()
  "Bracket XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-bracket 'xdigit nil))

(defalias 'ar-lesserangle-xdigit-atpt 'ar-xdigit-lesserangle-atpt)
(defun ar-xdigit-lesserangle-atpt ()
  "Lesserangle XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-lesserangle 'xdigit nil))

(defalias 'ar-greaterangle-xdigit-atpt 'ar-xdigit-greaterangle-atpt)
(defun ar-xdigit-greaterangle-atpt ()
  "Greaterangle XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-greaterangle 'xdigit nil))

(defalias 'ar-leftrightsinglequote-xdigit-atpt 'ar-xdigit-leftrightsinglequote-atpt)
(defun ar-xdigit-leftrightsinglequote-atpt ()
  "Leftrightsinglequote XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'xdigit nil))

(defalias 'ar-leftrightsinglequote-xdigit-atpt 'ar-xdigit-leftrightsinglequote-atpt)
(defun ar-xdigit-leftrightsinglequote-atpt ()
  "Leftrightsinglequote XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-leftrightsinglequote 'xdigit nil))

(defalias 'ar-parentize-xdigit-atpt 'ar-xdigit-parentize-atpt)
(defun ar-xdigit-parentize-atpt ()
  "Parentize XDIGIT at point if any, nil otherwise "
  (interactive "*")
  (ar-th-parentize 'xdigit nil));; ar-thing-at-point-utils-delim-classes-paired: end

;; ar-thing-at-point-utils-nodelim-einzeln: start

(defun ar-blok-alnum-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around alnum.
  Returns blok or nil if no ALNUM at cursor-position. "
  (interactive "*")
  (ar-th-blok 'alnum nil (interactive-p)))

(defun ar-comment-alnum-atpt ()
  "Comments ALNUM at point if any. "
  (interactive "*")
  (ar-th-comment 'alnum nil (interactive-p)))

(defun ar-commatize-alnum-atpt ()
  "Put a comma after ALNUM at point if any. "
  (interactive "*")
  (ar-th-commatize 'alnum nil (interactive-p)))

(defalias 'ar-mark-alnum-atpt 'ar-alnum-mark-atpt)
(defun ar-alnum-mark-atpt ()
  "Marks ALNUM at point if any. "
  (interactive)
  (ar-th-mark 'alnum))

(defalias 'ar-hide-alnum-atpt 'ar-alnum-hide-atpt)
(defun ar-alnum-hide-atpt ()
  "Hides ALNUM at point. "
  (interactive)
  (ar-th-hide 'alnum))

(defalias 'ar-show-alnum-atpt 'ar-alnum-show-atpt)
(defun ar-alnum-show-atpt ()
  "Shows hidden ALNUM at point. "
  (interactive)
  (ar-th-show 'alnum))

(defalias 'ar-hide-show-alnum-atpt 'ar-alnum-hide-show-atpt)
(defun ar-alnum-hide-show-atpt ()
  "Alternatively hides or shows ALNUM at point. "
  (interactive)
  (ar-th-hide-show 'alnum))

(defalias 'ar-highlight-alnum-atpt-mode 'ar-alnum-highlight-atpt-mode)

(defun ar-alnum-highlight-atpt-mode ()
  "Toggles alnum-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'alnum nil (interactive-p)))

(defalias 'ar-kill-alnum-atpt 'ar-alnum-kill-atpt)
(defun ar-alnum-kill-atpt ()
  "Kills ALNUM at point if any. "
  (interactive "*")
  (ar-th-kill 'alnum nil (interactive-p)))

(defalias 'ar-kill-backward-alnum-atpt 'ar-alnum-kill-backward-atpt)
(defun ar-alnum-kill-backward-atpt ()
  "Kills ALNUM at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'alnum nil (interactive-p)))

(defalias 'ar-separate-alnum-atpt 'ar-alnum-separate-atpt)
(defun ar-alnum-separate-atpt ()
  "Separates ALNUM at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'alnum nil (interactive-p)))

(defalias 'ar-triplequotedq-alnum-atpt 'ar-alnum-triplequotedq-atpt)
(defun ar-alnum-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around alnum. "
  (interactive "*")
  (ar-th-triplequotedq 'alnum nil (interactive-p)))

(defalias 'ar-triplequotesq-alnum-atpt 'ar-alnum-triplequotesq-atpt)
(defun ar-alnum-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around alnum. "
  (interactive "*")
  (ar-th-triplequotesq 'alnum nil (interactive-p)))

(defalias 'ar-forward-alnum-atpt 'ar-alnum-forward-atpt)
(defun ar-alnum-forward-atpt (&optional arg)
  "Moves forward over ALNUM at point if any, does nothing otherwise.
Returns end position of ALNUM "
  (interactive "p")
  (ar-th-forward 'alnum arg (interactive-p)))

(defalias 'ar-backward-alnum-atpt 'ar-alnum-backward-atpt)
(defun ar-alnum-backward-atpt (&optional arg)
  "Moves backward over ALNUM before point if any, does nothing otherwise.
Returns beginning position of ALNUM "
  (interactive "p")
  (ar-th-backward 'alnum arg (interactive-p)))

(defalias 'ar-transpose-alnum-atpt 'ar-alnum-transpose-atpt)
(defun ar-alnum-transpose-atpt (&optional arg)
  "Transposes ALNUM with ALNUM before point if any. "
  (interactive "*p")
  (ar-th-transpose 'alnum arg (interactive-p)))

(defalias 'ar-sort-alnum-atpt 'ar-alnum-sort-atpt)
(defun ar-alnum-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts alnums in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'alnum reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-alnum-atpt 'ar-alnum-check-atpt)
(defun ar-alnum-check-atpt ()
  "Return t if a ALNUM at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-alnum-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-alnum-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-alpha-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around alpha.
  Returns blok or nil if no ALPHA at cursor-position. "
  (interactive "*")
  (ar-th-blok 'alpha nil (interactive-p)))

(defun ar-comment-alpha-atpt ()
  "Comments ALPHA at point if any. "
  (interactive "*")
  (ar-th-comment 'alpha nil (interactive-p)))

(defun ar-commatize-alpha-atpt ()
  "Put a comma after ALPHA at point if any. "
  (interactive "*")
  (ar-th-commatize 'alpha nil (interactive-p)))

(defalias 'ar-mark-alpha-atpt 'ar-alpha-mark-atpt)
(defun ar-alpha-mark-atpt ()
  "Marks ALPHA at point if any. "
  (interactive)
  (ar-th-mark 'alpha))

(defalias 'ar-hide-alpha-atpt 'ar-alpha-hide-atpt)
(defun ar-alpha-hide-atpt ()
  "Hides ALPHA at point. "
  (interactive)
  (ar-th-hide 'alpha))

(defalias 'ar-show-alpha-atpt 'ar-alpha-show-atpt)
(defun ar-alpha-show-atpt ()
  "Shows hidden ALPHA at point. "
  (interactive)
  (ar-th-show 'alpha))

(defalias 'ar-hide-show-alpha-atpt 'ar-alpha-hide-show-atpt)
(defun ar-alpha-hide-show-atpt ()
  "Alternatively hides or shows ALPHA at point. "
  (interactive)
  (ar-th-hide-show 'alpha))

(defalias 'ar-highlight-alpha-atpt-mode 'ar-alpha-highlight-atpt-mode)

(defun ar-alpha-highlight-atpt-mode ()
  "Toggles alpha-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'alpha nil (interactive-p)))

(defalias 'ar-kill-alpha-atpt 'ar-alpha-kill-atpt)
(defun ar-alpha-kill-atpt ()
  "Kills ALPHA at point if any. "
  (interactive "*")
  (ar-th-kill 'alpha nil (interactive-p)))

(defalias 'ar-kill-backward-alpha-atpt 'ar-alpha-kill-backward-atpt)
(defun ar-alpha-kill-backward-atpt ()
  "Kills ALPHA at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'alpha nil (interactive-p)))

(defalias 'ar-separate-alpha-atpt 'ar-alpha-separate-atpt)
(defun ar-alpha-separate-atpt ()
  "Separates ALPHA at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'alpha nil (interactive-p)))

(defalias 'ar-triplequotedq-alpha-atpt 'ar-alpha-triplequotedq-atpt)
(defun ar-alpha-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around alpha. "
  (interactive "*")
  (ar-th-triplequotedq 'alpha nil (interactive-p)))

(defalias 'ar-triplequotesq-alpha-atpt 'ar-alpha-triplequotesq-atpt)
(defun ar-alpha-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around alpha. "
  (interactive "*")
  (ar-th-triplequotesq 'alpha nil (interactive-p)))

(defalias 'ar-forward-alpha-atpt 'ar-alpha-forward-atpt)
(defun ar-alpha-forward-atpt (&optional arg)
  "Moves forward over ALPHA at point if any, does nothing otherwise.
Returns end position of ALPHA "
  (interactive "p")
  (ar-th-forward 'alpha arg (interactive-p)))

(defalias 'ar-backward-alpha-atpt 'ar-alpha-backward-atpt)
(defun ar-alpha-backward-atpt (&optional arg)
  "Moves backward over ALPHA before point if any, does nothing otherwise.
Returns beginning position of ALPHA "
  (interactive "p")
  (ar-th-backward 'alpha arg (interactive-p)))

(defalias 'ar-transpose-alpha-atpt 'ar-alpha-transpose-atpt)
(defun ar-alpha-transpose-atpt (&optional arg)
  "Transposes ALPHA with ALPHA before point if any. "
  (interactive "*p")
  (ar-th-transpose 'alpha arg (interactive-p)))

(defalias 'ar-sort-alpha-atpt 'ar-alpha-sort-atpt)
(defun ar-alpha-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts alphas in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'alpha reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-alpha-atpt 'ar-alpha-check-atpt)
(defun ar-alpha-check-atpt ()
  "Return t if a ALPHA at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-alpha-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-alpha-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-ascii-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around ascii.
  Returns blok or nil if no ASCII at cursor-position. "
  (interactive "*")
  (ar-th-blok 'ascii nil (interactive-p)))

(defun ar-comment-ascii-atpt ()
  "Comments ASCII at point if any. "
  (interactive "*")
  (ar-th-comment 'ascii nil (interactive-p)))

(defun ar-commatize-ascii-atpt ()
  "Put a comma after ASCII at point if any. "
  (interactive "*")
  (ar-th-commatize 'ascii nil (interactive-p)))

(defalias 'ar-mark-ascii-atpt 'ar-ascii-mark-atpt)
(defun ar-ascii-mark-atpt ()
  "Marks ASCII at point if any. "
  (interactive)
  (ar-th-mark 'ascii))

(defalias 'ar-hide-ascii-atpt 'ar-ascii-hide-atpt)
(defun ar-ascii-hide-atpt ()
  "Hides ASCII at point. "
  (interactive)
  (ar-th-hide 'ascii))

(defalias 'ar-show-ascii-atpt 'ar-ascii-show-atpt)
(defun ar-ascii-show-atpt ()
  "Shows hidden ASCII at point. "
  (interactive)
  (ar-th-show 'ascii))

(defalias 'ar-hide-show-ascii-atpt 'ar-ascii-hide-show-atpt)
(defun ar-ascii-hide-show-atpt ()
  "Alternatively hides or shows ASCII at point. "
  (interactive)
  (ar-th-hide-show 'ascii))

(defalias 'ar-highlight-ascii-atpt-mode 'ar-ascii-highlight-atpt-mode)

(defun ar-ascii-highlight-atpt-mode ()
  "Toggles ascii-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'ascii nil (interactive-p)))

(defalias 'ar-kill-ascii-atpt 'ar-ascii-kill-atpt)
(defun ar-ascii-kill-atpt ()
  "Kills ASCII at point if any. "
  (interactive "*")
  (ar-th-kill 'ascii nil (interactive-p)))

(defalias 'ar-kill-backward-ascii-atpt 'ar-ascii-kill-backward-atpt)
(defun ar-ascii-kill-backward-atpt ()
  "Kills ASCII at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'ascii nil (interactive-p)))

(defalias 'ar-separate-ascii-atpt 'ar-ascii-separate-atpt)
(defun ar-ascii-separate-atpt ()
  "Separates ASCII at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'ascii nil (interactive-p)))

(defalias 'ar-triplequotedq-ascii-atpt 'ar-ascii-triplequotedq-atpt)
(defun ar-ascii-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around ascii. "
  (interactive "*")
  (ar-th-triplequotedq 'ascii nil (interactive-p)))

(defalias 'ar-triplequotesq-ascii-atpt 'ar-ascii-triplequotesq-atpt)
(defun ar-ascii-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around ascii. "
  (interactive "*")
  (ar-th-triplequotesq 'ascii nil (interactive-p)))

(defalias 'ar-forward-ascii-atpt 'ar-ascii-forward-atpt)
(defun ar-ascii-forward-atpt (&optional arg)
  "Moves forward over ASCII at point if any, does nothing otherwise.
Returns end position of ASCII "
  (interactive "p")
  (ar-th-forward 'ascii arg (interactive-p)))

(defalias 'ar-backward-ascii-atpt 'ar-ascii-backward-atpt)
(defun ar-ascii-backward-atpt (&optional arg)
  "Moves backward over ASCII before point if any, does nothing otherwise.
Returns beginning position of ASCII "
  (interactive "p")
  (ar-th-backward 'ascii arg (interactive-p)))

(defalias 'ar-transpose-ascii-atpt 'ar-ascii-transpose-atpt)
(defun ar-ascii-transpose-atpt (&optional arg)
  "Transposes ASCII with ASCII before point if any. "
  (interactive "*p")
  (ar-th-transpose 'ascii arg (interactive-p)))

(defalias 'ar-sort-ascii-atpt 'ar-ascii-sort-atpt)
(defun ar-ascii-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts asciis in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'ascii reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-ascii-atpt 'ar-ascii-check-atpt)
(defun ar-ascii-check-atpt ()
  "Return t if a ASCII at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-ascii-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-ascii-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-blank-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around blank.
  Returns blok or nil if no BLANK at cursor-position. "
  (interactive "*")
  (ar-th-blok 'blank nil (interactive-p)))

(defun ar-comment-blank-atpt ()
  "Comments BLANK at point if any. "
  (interactive "*")
  (ar-th-comment 'blank nil (interactive-p)))

(defun ar-commatize-blank-atpt ()
  "Put a comma after BLANK at point if any. "
  (interactive "*")
  (ar-th-commatize 'blank nil (interactive-p)))

(defalias 'ar-mark-blank-atpt 'ar-blank-mark-atpt)
(defun ar-blank-mark-atpt ()
  "Marks BLANK at point if any. "
  (interactive)
  (ar-th-mark 'blank))

(defalias 'ar-hide-blank-atpt 'ar-blank-hide-atpt)
(defun ar-blank-hide-atpt ()
  "Hides BLANK at point. "
  (interactive)
  (ar-th-hide 'blank))

(defalias 'ar-show-blank-atpt 'ar-blank-show-atpt)
(defun ar-blank-show-atpt ()
  "Shows hidden BLANK at point. "
  (interactive)
  (ar-th-show 'blank))

(defalias 'ar-hide-show-blank-atpt 'ar-blank-hide-show-atpt)
(defun ar-blank-hide-show-atpt ()
  "Alternatively hides or shows BLANK at point. "
  (interactive)
  (ar-th-hide-show 'blank))

(defalias 'ar-highlight-blank-atpt-mode 'ar-blank-highlight-atpt-mode)

(defun ar-blank-highlight-atpt-mode ()
  "Toggles blank-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'blank nil (interactive-p)))

(defalias 'ar-kill-blank-atpt 'ar-blank-kill-atpt)
(defun ar-blank-kill-atpt ()
  "Kills BLANK at point if any. "
  (interactive "*")
  (ar-th-kill 'blank nil (interactive-p)))

(defalias 'ar-kill-backward-blank-atpt 'ar-blank-kill-backward-atpt)
(defun ar-blank-kill-backward-atpt ()
  "Kills BLANK at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'blank nil (interactive-p)))

(defalias 'ar-separate-blank-atpt 'ar-blank-separate-atpt)
(defun ar-blank-separate-atpt ()
  "Separates BLANK at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'blank nil (interactive-p)))

(defalias 'ar-triplequotedq-blank-atpt 'ar-blank-triplequotedq-atpt)
(defun ar-blank-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around blank. "
  (interactive "*")
  (ar-th-triplequotedq 'blank nil (interactive-p)))

(defalias 'ar-triplequotesq-blank-atpt 'ar-blank-triplequotesq-atpt)
(defun ar-blank-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around blank. "
  (interactive "*")
  (ar-th-triplequotesq 'blank nil (interactive-p)))

(defalias 'ar-forward-blank-atpt 'ar-blank-forward-atpt)
(defun ar-blank-forward-atpt (&optional arg)
  "Moves forward over BLANK at point if any, does nothing otherwise.
Returns end position of BLANK "
  (interactive "p")
  (ar-th-forward 'blank arg (interactive-p)))

(defalias 'ar-backward-blank-atpt 'ar-blank-backward-atpt)
(defun ar-blank-backward-atpt (&optional arg)
  "Moves backward over BLANK before point if any, does nothing otherwise.
Returns beginning position of BLANK "
  (interactive "p")
  (ar-th-backward 'blank arg (interactive-p)))

(defalias 'ar-transpose-blank-atpt 'ar-blank-transpose-atpt)
(defun ar-blank-transpose-atpt (&optional arg)
  "Transposes BLANK with BLANK before point if any. "
  (interactive "*p")
  (ar-th-transpose 'blank arg (interactive-p)))

(defalias 'ar-sort-blank-atpt 'ar-blank-sort-atpt)
(defun ar-blank-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts blanks in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'blank reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-blank-atpt 'ar-blank-check-atpt)
(defun ar-blank-check-atpt ()
  "Return t if a BLANK at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-blank-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-blank-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-cntrl-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around cntrl.
  Returns blok or nil if no CNTRL at cursor-position. "
  (interactive "*")
  (ar-th-blok 'cntrl nil (interactive-p)))

(defun ar-comment-cntrl-atpt ()
  "Comments CNTRL at point if any. "
  (interactive "*")
  (ar-th-comment 'cntrl nil (interactive-p)))

(defun ar-commatize-cntrl-atpt ()
  "Put a comma after CNTRL at point if any. "
  (interactive "*")
  (ar-th-commatize 'cntrl nil (interactive-p)))

(defalias 'ar-mark-cntrl-atpt 'ar-cntrl-mark-atpt)
(defun ar-cntrl-mark-atpt ()
  "Marks CNTRL at point if any. "
  (interactive)
  (ar-th-mark 'cntrl))

(defalias 'ar-hide-cntrl-atpt 'ar-cntrl-hide-atpt)
(defun ar-cntrl-hide-atpt ()
  "Hides CNTRL at point. "
  (interactive)
  (ar-th-hide 'cntrl))

(defalias 'ar-show-cntrl-atpt 'ar-cntrl-show-atpt)
(defun ar-cntrl-show-atpt ()
  "Shows hidden CNTRL at point. "
  (interactive)
  (ar-th-show 'cntrl))

(defalias 'ar-hide-show-cntrl-atpt 'ar-cntrl-hide-show-atpt)
(defun ar-cntrl-hide-show-atpt ()
  "Alternatively hides or shows CNTRL at point. "
  (interactive)
  (ar-th-hide-show 'cntrl))

(defalias 'ar-highlight-cntrl-atpt-mode 'ar-cntrl-highlight-atpt-mode)

(defun ar-cntrl-highlight-atpt-mode ()
  "Toggles cntrl-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'cntrl nil (interactive-p)))

(defalias 'ar-kill-cntrl-atpt 'ar-cntrl-kill-atpt)
(defun ar-cntrl-kill-atpt ()
  "Kills CNTRL at point if any. "
  (interactive "*")
  (ar-th-kill 'cntrl nil (interactive-p)))

(defalias 'ar-kill-backward-cntrl-atpt 'ar-cntrl-kill-backward-atpt)
(defun ar-cntrl-kill-backward-atpt ()
  "Kills CNTRL at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'cntrl nil (interactive-p)))

(defalias 'ar-separate-cntrl-atpt 'ar-cntrl-separate-atpt)
(defun ar-cntrl-separate-atpt ()
  "Separates CNTRL at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'cntrl nil (interactive-p)))

(defalias 'ar-triplequotedq-cntrl-atpt 'ar-cntrl-triplequotedq-atpt)
(defun ar-cntrl-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around cntrl. "
  (interactive "*")
  (ar-th-triplequotedq 'cntrl nil (interactive-p)))

(defalias 'ar-triplequotesq-cntrl-atpt 'ar-cntrl-triplequotesq-atpt)
(defun ar-cntrl-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around cntrl. "
  (interactive "*")
  (ar-th-triplequotesq 'cntrl nil (interactive-p)))

(defalias 'ar-forward-cntrl-atpt 'ar-cntrl-forward-atpt)
(defun ar-cntrl-forward-atpt (&optional arg)
  "Moves forward over CNTRL at point if any, does nothing otherwise.
Returns end position of CNTRL "
  (interactive "p")
  (ar-th-forward 'cntrl arg (interactive-p)))

(defalias 'ar-backward-cntrl-atpt 'ar-cntrl-backward-atpt)
(defun ar-cntrl-backward-atpt (&optional arg)
  "Moves backward over CNTRL before point if any, does nothing otherwise.
Returns beginning position of CNTRL "
  (interactive "p")
  (ar-th-backward 'cntrl arg (interactive-p)))

(defalias 'ar-transpose-cntrl-atpt 'ar-cntrl-transpose-atpt)
(defun ar-cntrl-transpose-atpt (&optional arg)
  "Transposes CNTRL with CNTRL before point if any. "
  (interactive "*p")
  (ar-th-transpose 'cntrl arg (interactive-p)))

(defalias 'ar-sort-cntrl-atpt 'ar-cntrl-sort-atpt)
(defun ar-cntrl-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts cntrls in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'cntrl reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-cntrl-atpt 'ar-cntrl-check-atpt)
(defun ar-cntrl-check-atpt ()
  "Return t if a CNTRL at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-cntrl-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-cntrl-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-digit-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around digit.
  Returns blok or nil if no DIGIT at cursor-position. "
  (interactive "*")
  (ar-th-blok 'digit nil (interactive-p)))

(defun ar-comment-digit-atpt ()
  "Comments DIGIT at point if any. "
  (interactive "*")
  (ar-th-comment 'digit nil (interactive-p)))

(defun ar-commatize-digit-atpt ()
  "Put a comma after DIGIT at point if any. "
  (interactive "*")
  (ar-th-commatize 'digit nil (interactive-p)))

(defalias 'ar-mark-digit-atpt 'ar-digit-mark-atpt)
(defun ar-digit-mark-atpt ()
  "Marks DIGIT at point if any. "
  (interactive)
  (ar-th-mark 'digit))

(defalias 'ar-hide-digit-atpt 'ar-digit-hide-atpt)
(defun ar-digit-hide-atpt ()
  "Hides DIGIT at point. "
  (interactive)
  (ar-th-hide 'digit))

(defalias 'ar-show-digit-atpt 'ar-digit-show-atpt)
(defun ar-digit-show-atpt ()
  "Shows hidden DIGIT at point. "
  (interactive)
  (ar-th-show 'digit))

(defalias 'ar-hide-show-digit-atpt 'ar-digit-hide-show-atpt)
(defun ar-digit-hide-show-atpt ()
  "Alternatively hides or shows DIGIT at point. "
  (interactive)
  (ar-th-hide-show 'digit))

(defalias 'ar-highlight-digit-atpt-mode 'ar-digit-highlight-atpt-mode)

(defun ar-digit-highlight-atpt-mode ()
  "Toggles digit-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'digit nil (interactive-p)))

(defalias 'ar-kill-digit-atpt 'ar-digit-kill-atpt)
(defun ar-digit-kill-atpt ()
  "Kills DIGIT at point if any. "
  (interactive "*")
  (ar-th-kill 'digit nil (interactive-p)))

(defalias 'ar-kill-backward-digit-atpt 'ar-digit-kill-backward-atpt)
(defun ar-digit-kill-backward-atpt ()
  "Kills DIGIT at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'digit nil (interactive-p)))

(defalias 'ar-separate-digit-atpt 'ar-digit-separate-atpt)
(defun ar-digit-separate-atpt ()
  "Separates DIGIT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'digit nil (interactive-p)))

(defalias 'ar-triplequotedq-digit-atpt 'ar-digit-triplequotedq-atpt)
(defun ar-digit-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around digit. "
  (interactive "*")
  (ar-th-triplequotedq 'digit nil (interactive-p)))

(defalias 'ar-triplequotesq-digit-atpt 'ar-digit-triplequotesq-atpt)
(defun ar-digit-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around digit. "
  (interactive "*")
  (ar-th-triplequotesq 'digit nil (interactive-p)))

(defalias 'ar-forward-digit-atpt 'ar-digit-forward-atpt)
(defun ar-digit-forward-atpt (&optional arg)
  "Moves forward over DIGIT at point if any, does nothing otherwise.
Returns end position of DIGIT "
  (interactive "p")
  (ar-th-forward 'digit arg (interactive-p)))

(defalias 'ar-backward-digit-atpt 'ar-digit-backward-atpt)
(defun ar-digit-backward-atpt (&optional arg)
  "Moves backward over DIGIT before point if any, does nothing otherwise.
Returns beginning position of DIGIT "
  (interactive "p")
  (ar-th-backward 'digit arg (interactive-p)))

(defalias 'ar-transpose-digit-atpt 'ar-digit-transpose-atpt)
(defun ar-digit-transpose-atpt (&optional arg)
  "Transposes DIGIT with DIGIT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'digit arg (interactive-p)))

(defalias 'ar-sort-digit-atpt 'ar-digit-sort-atpt)
(defun ar-digit-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts digits in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'digit reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-digit-atpt 'ar-digit-check-atpt)
(defun ar-digit-check-atpt ()
  "Return t if a DIGIT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-digit-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-digit-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-graph-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around graph.
  Returns blok or nil if no GRAPH at cursor-position. "
  (interactive "*")
  (ar-th-blok 'graph nil (interactive-p)))

(defun ar-comment-graph-atpt ()
  "Comments GRAPH at point if any. "
  (interactive "*")
  (ar-th-comment 'graph nil (interactive-p)))

(defun ar-commatize-graph-atpt ()
  "Put a comma after GRAPH at point if any. "
  (interactive "*")
  (ar-th-commatize 'graph nil (interactive-p)))

(defalias 'ar-mark-graph-atpt 'ar-graph-mark-atpt)
(defun ar-graph-mark-atpt ()
  "Marks GRAPH at point if any. "
  (interactive)
  (ar-th-mark 'graph))

(defalias 'ar-hide-graph-atpt 'ar-graph-hide-atpt)
(defun ar-graph-hide-atpt ()
  "Hides GRAPH at point. "
  (interactive)
  (ar-th-hide 'graph))

(defalias 'ar-show-graph-atpt 'ar-graph-show-atpt)
(defun ar-graph-show-atpt ()
  "Shows hidden GRAPH at point. "
  (interactive)
  (ar-th-show 'graph))

(defalias 'ar-hide-show-graph-atpt 'ar-graph-hide-show-atpt)
(defun ar-graph-hide-show-atpt ()
  "Alternatively hides or shows GRAPH at point. "
  (interactive)
  (ar-th-hide-show 'graph))

(defalias 'ar-highlight-graph-atpt-mode 'ar-graph-highlight-atpt-mode)

(defun ar-graph-highlight-atpt-mode ()
  "Toggles graph-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'graph nil (interactive-p)))

(defalias 'ar-kill-graph-atpt 'ar-graph-kill-atpt)
(defun ar-graph-kill-atpt ()
  "Kills GRAPH at point if any. "
  (interactive "*")
  (ar-th-kill 'graph nil (interactive-p)))

(defalias 'ar-kill-backward-graph-atpt 'ar-graph-kill-backward-atpt)
(defun ar-graph-kill-backward-atpt ()
  "Kills GRAPH at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'graph nil (interactive-p)))

(defalias 'ar-separate-graph-atpt 'ar-graph-separate-atpt)
(defun ar-graph-separate-atpt ()
  "Separates GRAPH at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'graph nil (interactive-p)))

(defalias 'ar-triplequotedq-graph-atpt 'ar-graph-triplequotedq-atpt)
(defun ar-graph-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around graph. "
  (interactive "*")
  (ar-th-triplequotedq 'graph nil (interactive-p)))

(defalias 'ar-triplequotesq-graph-atpt 'ar-graph-triplequotesq-atpt)
(defun ar-graph-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around graph. "
  (interactive "*")
  (ar-th-triplequotesq 'graph nil (interactive-p)))

(defalias 'ar-forward-graph-atpt 'ar-graph-forward-atpt)
(defun ar-graph-forward-atpt (&optional arg)
  "Moves forward over GRAPH at point if any, does nothing otherwise.
Returns end position of GRAPH "
  (interactive "p")
  (ar-th-forward 'graph arg (interactive-p)))

(defalias 'ar-backward-graph-atpt 'ar-graph-backward-atpt)
(defun ar-graph-backward-atpt (&optional arg)
  "Moves backward over GRAPH before point if any, does nothing otherwise.
Returns beginning position of GRAPH "
  (interactive "p")
  (ar-th-backward 'graph arg (interactive-p)))

(defalias 'ar-transpose-graph-atpt 'ar-graph-transpose-atpt)
(defun ar-graph-transpose-atpt (&optional arg)
  "Transposes GRAPH with GRAPH before point if any. "
  (interactive "*p")
  (ar-th-transpose 'graph arg (interactive-p)))

(defalias 'ar-sort-graph-atpt 'ar-graph-sort-atpt)
(defun ar-graph-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts graphs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'graph reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-graph-atpt 'ar-graph-check-atpt)
(defun ar-graph-check-atpt ()
  "Return t if a GRAPH at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-graph-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-graph-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-lower-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around lower.
  Returns blok or nil if no LOWER at cursor-position. "
  (interactive "*")
  (ar-th-blok 'lower nil (interactive-p)))

(defun ar-comment-lower-atpt ()
  "Comments LOWER at point if any. "
  (interactive "*")
  (ar-th-comment 'lower nil (interactive-p)))

(defun ar-commatize-lower-atpt ()
  "Put a comma after LOWER at point if any. "
  (interactive "*")
  (ar-th-commatize 'lower nil (interactive-p)))

(defalias 'ar-mark-lower-atpt 'ar-lower-mark-atpt)
(defun ar-lower-mark-atpt ()
  "Marks LOWER at point if any. "
  (interactive)
  (ar-th-mark 'lower))

(defalias 'ar-hide-lower-atpt 'ar-lower-hide-atpt)
(defun ar-lower-hide-atpt ()
  "Hides LOWER at point. "
  (interactive)
  (ar-th-hide 'lower))

(defalias 'ar-show-lower-atpt 'ar-lower-show-atpt)
(defun ar-lower-show-atpt ()
  "Shows hidden LOWER at point. "
  (interactive)
  (ar-th-show 'lower))

(defalias 'ar-hide-show-lower-atpt 'ar-lower-hide-show-atpt)
(defun ar-lower-hide-show-atpt ()
  "Alternatively hides or shows LOWER at point. "
  (interactive)
  (ar-th-hide-show 'lower))

(defalias 'ar-highlight-lower-atpt-mode 'ar-lower-highlight-atpt-mode)

(defun ar-lower-highlight-atpt-mode ()
  "Toggles lower-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'lower nil (interactive-p)))

(defalias 'ar-kill-lower-atpt 'ar-lower-kill-atpt)
(defun ar-lower-kill-atpt ()
  "Kills LOWER at point if any. "
  (interactive "*")
  (ar-th-kill 'lower nil (interactive-p)))

(defalias 'ar-kill-backward-lower-atpt 'ar-lower-kill-backward-atpt)
(defun ar-lower-kill-backward-atpt ()
  "Kills LOWER at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'lower nil (interactive-p)))

(defalias 'ar-separate-lower-atpt 'ar-lower-separate-atpt)
(defun ar-lower-separate-atpt ()
  "Separates LOWER at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'lower nil (interactive-p)))

(defalias 'ar-triplequotedq-lower-atpt 'ar-lower-triplequotedq-atpt)
(defun ar-lower-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around lower. "
  (interactive "*")
  (ar-th-triplequotedq 'lower nil (interactive-p)))

(defalias 'ar-triplequotesq-lower-atpt 'ar-lower-triplequotesq-atpt)
(defun ar-lower-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around lower. "
  (interactive "*")
  (ar-th-triplequotesq 'lower nil (interactive-p)))

(defalias 'ar-forward-lower-atpt 'ar-lower-forward-atpt)
(defun ar-lower-forward-atpt (&optional arg)
  "Moves forward over LOWER at point if any, does nothing otherwise.
Returns end position of LOWER "
  (interactive "p")
  (ar-th-forward 'lower arg (interactive-p)))

(defalias 'ar-backward-lower-atpt 'ar-lower-backward-atpt)
(defun ar-lower-backward-atpt (&optional arg)
  "Moves backward over LOWER before point if any, does nothing otherwise.
Returns beginning position of LOWER "
  (interactive "p")
  (ar-th-backward 'lower arg (interactive-p)))

(defalias 'ar-transpose-lower-atpt 'ar-lower-transpose-atpt)
(defun ar-lower-transpose-atpt (&optional arg)
  "Transposes LOWER with LOWER before point if any. "
  (interactive "*p")
  (ar-th-transpose 'lower arg (interactive-p)))

(defalias 'ar-sort-lower-atpt 'ar-lower-sort-atpt)
(defun ar-lower-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lowers in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'lower reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-lower-atpt 'ar-lower-check-atpt)
(defun ar-lower-check-atpt ()
  "Return t if a LOWER at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-lower-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-lower-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-nonascii-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around nonascii.
  Returns blok or nil if no NONASCII at cursor-position. "
  (interactive "*")
  (ar-th-blok 'nonascii nil (interactive-p)))

(defun ar-comment-nonascii-atpt ()
  "Comments NONASCII at point if any. "
  (interactive "*")
  (ar-th-comment 'nonascii nil (interactive-p)))

(defun ar-commatize-nonascii-atpt ()
  "Put a comma after NONASCII at point if any. "
  (interactive "*")
  (ar-th-commatize 'nonascii nil (interactive-p)))

(defalias 'ar-mark-nonascii-atpt 'ar-nonascii-mark-atpt)
(defun ar-nonascii-mark-atpt ()
  "Marks NONASCII at point if any. "
  (interactive)
  (ar-th-mark 'nonascii))

(defalias 'ar-hide-nonascii-atpt 'ar-nonascii-hide-atpt)
(defun ar-nonascii-hide-atpt ()
  "Hides NONASCII at point. "
  (interactive)
  (ar-th-hide 'nonascii))

(defalias 'ar-show-nonascii-atpt 'ar-nonascii-show-atpt)
(defun ar-nonascii-show-atpt ()
  "Shows hidden NONASCII at point. "
  (interactive)
  (ar-th-show 'nonascii))

(defalias 'ar-hide-show-nonascii-atpt 'ar-nonascii-hide-show-atpt)
(defun ar-nonascii-hide-show-atpt ()
  "Alternatively hides or shows NONASCII at point. "
  (interactive)
  (ar-th-hide-show 'nonascii))

(defalias 'ar-highlight-nonascii-atpt-mode 'ar-nonascii-highlight-atpt-mode)

(defun ar-nonascii-highlight-atpt-mode ()
  "Toggles nonascii-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'nonascii nil (interactive-p)))

(defalias 'ar-kill-nonascii-atpt 'ar-nonascii-kill-atpt)
(defun ar-nonascii-kill-atpt ()
  "Kills NONASCII at point if any. "
  (interactive "*")
  (ar-th-kill 'nonascii nil (interactive-p)))

(defalias 'ar-kill-backward-nonascii-atpt 'ar-nonascii-kill-backward-atpt)
(defun ar-nonascii-kill-backward-atpt ()
  "Kills NONASCII at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'nonascii nil (interactive-p)))

(defalias 'ar-separate-nonascii-atpt 'ar-nonascii-separate-atpt)
(defun ar-nonascii-separate-atpt ()
  "Separates NONASCII at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'nonascii nil (interactive-p)))

(defalias 'ar-triplequotedq-nonascii-atpt 'ar-nonascii-triplequotedq-atpt)
(defun ar-nonascii-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around nonascii. "
  (interactive "*")
  (ar-th-triplequotedq 'nonascii nil (interactive-p)))

(defalias 'ar-triplequotesq-nonascii-atpt 'ar-nonascii-triplequotesq-atpt)
(defun ar-nonascii-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around nonascii. "
  (interactive "*")
  (ar-th-triplequotesq 'nonascii nil (interactive-p)))

(defalias 'ar-forward-nonascii-atpt 'ar-nonascii-forward-atpt)
(defun ar-nonascii-forward-atpt (&optional arg)
  "Moves forward over NONASCII at point if any, does nothing otherwise.
Returns end position of NONASCII "
  (interactive "p")
  (ar-th-forward 'nonascii arg (interactive-p)))

(defalias 'ar-backward-nonascii-atpt 'ar-nonascii-backward-atpt)
(defun ar-nonascii-backward-atpt (&optional arg)
  "Moves backward over NONASCII before point if any, does nothing otherwise.
Returns beginning position of NONASCII "
  (interactive "p")
  (ar-th-backward 'nonascii arg (interactive-p)))

(defalias 'ar-transpose-nonascii-atpt 'ar-nonascii-transpose-atpt)
(defun ar-nonascii-transpose-atpt (&optional arg)
  "Transposes NONASCII with NONASCII before point if any. "
  (interactive "*p")
  (ar-th-transpose 'nonascii arg (interactive-p)))

(defalias 'ar-sort-nonascii-atpt 'ar-nonascii-sort-atpt)
(defun ar-nonascii-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts nonasciis in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'nonascii reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-nonascii-atpt 'ar-nonascii-check-atpt)
(defun ar-nonascii-check-atpt ()
  "Return t if a NONASCII at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-nonascii-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-nonascii-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-print-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around print.
  Returns blok or nil if no PRINT at cursor-position. "
  (interactive "*")
  (ar-th-blok 'print nil (interactive-p)))

(defun ar-comment-print-atpt ()
  "Comments PRINT at point if any. "
  (interactive "*")
  (ar-th-comment 'print nil (interactive-p)))

(defun ar-commatize-print-atpt ()
  "Put a comma after PRINT at point if any. "
  (interactive "*")
  (ar-th-commatize 'print nil (interactive-p)))

(defalias 'ar-mark-print-atpt 'ar-print-mark-atpt)
(defun ar-print-mark-atpt ()
  "Marks PRINT at point if any. "
  (interactive)
  (ar-th-mark 'print))

(defalias 'ar-hide-print-atpt 'ar-print-hide-atpt)
(defun ar-print-hide-atpt ()
  "Hides PRINT at point. "
  (interactive)
  (ar-th-hide 'print))

(defalias 'ar-show-print-atpt 'ar-print-show-atpt)
(defun ar-print-show-atpt ()
  "Shows hidden PRINT at point. "
  (interactive)
  (ar-th-show 'print))

(defalias 'ar-hide-show-print-atpt 'ar-print-hide-show-atpt)
(defun ar-print-hide-show-atpt ()
  "Alternatively hides or shows PRINT at point. "
  (interactive)
  (ar-th-hide-show 'print))

(defalias 'ar-highlight-print-atpt-mode 'ar-print-highlight-atpt-mode)

(defun ar-print-highlight-atpt-mode ()
  "Toggles print-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'print nil (interactive-p)))

(defalias 'ar-kill-print-atpt 'ar-print-kill-atpt)
(defun ar-print-kill-atpt ()
  "Kills PRINT at point if any. "
  (interactive "*")
  (ar-th-kill 'print nil (interactive-p)))

(defalias 'ar-kill-backward-print-atpt 'ar-print-kill-backward-atpt)
(defun ar-print-kill-backward-atpt ()
  "Kills PRINT at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'print nil (interactive-p)))

(defalias 'ar-separate-print-atpt 'ar-print-separate-atpt)
(defun ar-print-separate-atpt ()
  "Separates PRINT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'print nil (interactive-p)))

(defalias 'ar-triplequotedq-print-atpt 'ar-print-triplequotedq-atpt)
(defun ar-print-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around print. "
  (interactive "*")
  (ar-th-triplequotedq 'print nil (interactive-p)))

(defalias 'ar-triplequotesq-print-atpt 'ar-print-triplequotesq-atpt)
(defun ar-print-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around print. "
  (interactive "*")
  (ar-th-triplequotesq 'print nil (interactive-p)))

(defalias 'ar-forward-print-atpt 'ar-print-forward-atpt)
(defun ar-print-forward-atpt (&optional arg)
  "Moves forward over PRINT at point if any, does nothing otherwise.
Returns end position of PRINT "
  (interactive "p")
  (ar-th-forward 'print arg (interactive-p)))

(defalias 'ar-backward-print-atpt 'ar-print-backward-atpt)
(defun ar-print-backward-atpt (&optional arg)
  "Moves backward over PRINT before point if any, does nothing otherwise.
Returns beginning position of PRINT "
  (interactive "p")
  (ar-th-backward 'print arg (interactive-p)))

(defalias 'ar-transpose-print-atpt 'ar-print-transpose-atpt)
(defun ar-print-transpose-atpt (&optional arg)
  "Transposes PRINT with PRINT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'print arg (interactive-p)))

(defalias 'ar-sort-print-atpt 'ar-print-sort-atpt)
(defun ar-print-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts prints in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'print reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-print-atpt 'ar-print-check-atpt)
(defun ar-print-check-atpt ()
  "Return t if a PRINT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-print-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-print-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-punct-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around punct.
  Returns blok or nil if no PUNCT at cursor-position. "
  (interactive "*")
  (ar-th-blok 'punct nil (interactive-p)))

(defun ar-comment-punct-atpt ()
  "Comments PUNCT at point if any. "
  (interactive "*")
  (ar-th-comment 'punct nil (interactive-p)))

(defun ar-commatize-punct-atpt ()
  "Put a comma after PUNCT at point if any. "
  (interactive "*")
  (ar-th-commatize 'punct nil (interactive-p)))

(defalias 'ar-mark-punct-atpt 'ar-punct-mark-atpt)
(defun ar-punct-mark-atpt ()
  "Marks PUNCT at point if any. "
  (interactive)
  (ar-th-mark 'punct))

(defalias 'ar-hide-punct-atpt 'ar-punct-hide-atpt)
(defun ar-punct-hide-atpt ()
  "Hides PUNCT at point. "
  (interactive)
  (ar-th-hide 'punct))

(defalias 'ar-show-punct-atpt 'ar-punct-show-atpt)
(defun ar-punct-show-atpt ()
  "Shows hidden PUNCT at point. "
  (interactive)
  (ar-th-show 'punct))

(defalias 'ar-hide-show-punct-atpt 'ar-punct-hide-show-atpt)
(defun ar-punct-hide-show-atpt ()
  "Alternatively hides or shows PUNCT at point. "
  (interactive)
  (ar-th-hide-show 'punct))

(defalias 'ar-highlight-punct-atpt-mode 'ar-punct-highlight-atpt-mode)

(defun ar-punct-highlight-atpt-mode ()
  "Toggles punct-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'punct nil (interactive-p)))

(defalias 'ar-kill-punct-atpt 'ar-punct-kill-atpt)
(defun ar-punct-kill-atpt ()
  "Kills PUNCT at point if any. "
  (interactive "*")
  (ar-th-kill 'punct nil (interactive-p)))

(defalias 'ar-kill-backward-punct-atpt 'ar-punct-kill-backward-atpt)
(defun ar-punct-kill-backward-atpt ()
  "Kills PUNCT at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'punct nil (interactive-p)))

(defalias 'ar-separate-punct-atpt 'ar-punct-separate-atpt)
(defun ar-punct-separate-atpt ()
  "Separates PUNCT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'punct nil (interactive-p)))

(defalias 'ar-triplequotedq-punct-atpt 'ar-punct-triplequotedq-atpt)
(defun ar-punct-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around punct. "
  (interactive "*")
  (ar-th-triplequotedq 'punct nil (interactive-p)))

(defalias 'ar-triplequotesq-punct-atpt 'ar-punct-triplequotesq-atpt)
(defun ar-punct-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around punct. "
  (interactive "*")
  (ar-th-triplequotesq 'punct nil (interactive-p)))

(defalias 'ar-forward-punct-atpt 'ar-punct-forward-atpt)
(defun ar-punct-forward-atpt (&optional arg)
  "Moves forward over PUNCT at point if any, does nothing otherwise.
Returns end position of PUNCT "
  (interactive "p")
  (ar-th-forward 'punct arg (interactive-p)))

(defalias 'ar-backward-punct-atpt 'ar-punct-backward-atpt)
(defun ar-punct-backward-atpt (&optional arg)
  "Moves backward over PUNCT before point if any, does nothing otherwise.
Returns beginning position of PUNCT "
  (interactive "p")
  (ar-th-backward 'punct arg (interactive-p)))

(defalias 'ar-transpose-punct-atpt 'ar-punct-transpose-atpt)
(defun ar-punct-transpose-atpt (&optional arg)
  "Transposes PUNCT with PUNCT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'punct arg (interactive-p)))

(defalias 'ar-sort-punct-atpt 'ar-punct-sort-atpt)
(defun ar-punct-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts puncts in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'punct reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-punct-atpt 'ar-punct-check-atpt)
(defun ar-punct-check-atpt ()
  "Return t if a PUNCT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-punct-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-punct-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-space-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around space.
  Returns blok or nil if no SPACE at cursor-position. "
  (interactive "*")
  (ar-th-blok 'space nil (interactive-p)))

(defun ar-comment-space-atpt ()
  "Comments SPACE at point if any. "
  (interactive "*")
  (ar-th-comment 'space nil (interactive-p)))

(defun ar-commatize-space-atpt ()
  "Put a comma after SPACE at point if any. "
  (interactive "*")
  (ar-th-commatize 'space nil (interactive-p)))

(defalias 'ar-mark-space-atpt 'ar-space-mark-atpt)
(defun ar-space-mark-atpt ()
  "Marks SPACE at point if any. "
  (interactive)
  (ar-th-mark 'space))

(defalias 'ar-hide-space-atpt 'ar-space-hide-atpt)
(defun ar-space-hide-atpt ()
  "Hides SPACE at point. "
  (interactive)
  (ar-th-hide 'space))

(defalias 'ar-show-space-atpt 'ar-space-show-atpt)
(defun ar-space-show-atpt ()
  "Shows hidden SPACE at point. "
  (interactive)
  (ar-th-show 'space))

(defalias 'ar-hide-show-space-atpt 'ar-space-hide-show-atpt)
(defun ar-space-hide-show-atpt ()
  "Alternatively hides or shows SPACE at point. "
  (interactive)
  (ar-th-hide-show 'space))

(defalias 'ar-highlight-space-atpt-mode 'ar-space-highlight-atpt-mode)

(defun ar-space-highlight-atpt-mode ()
  "Toggles space-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'space nil (interactive-p)))

(defalias 'ar-kill-space-atpt 'ar-space-kill-atpt)
(defun ar-space-kill-atpt ()
  "Kills SPACE at point if any. "
  (interactive "*")
  (ar-th-kill 'space nil (interactive-p)))

(defalias 'ar-kill-backward-space-atpt 'ar-space-kill-backward-atpt)
(defun ar-space-kill-backward-atpt ()
  "Kills SPACE at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'space nil (interactive-p)))

(defalias 'ar-separate-space-atpt 'ar-space-separate-atpt)
(defun ar-space-separate-atpt ()
  "Separates SPACE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'space nil (interactive-p)))

(defalias 'ar-triplequotedq-space-atpt 'ar-space-triplequotedq-atpt)
(defun ar-space-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around space. "
  (interactive "*")
  (ar-th-triplequotedq 'space nil (interactive-p)))

(defalias 'ar-triplequotesq-space-atpt 'ar-space-triplequotesq-atpt)
(defun ar-space-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around space. "
  (interactive "*")
  (ar-th-triplequotesq 'space nil (interactive-p)))

(defalias 'ar-forward-space-atpt 'ar-space-forward-atpt)
(defun ar-space-forward-atpt (&optional arg)
  "Moves forward over SPACE at point if any, does nothing otherwise.
Returns end position of SPACE "
  (interactive "p")
  (ar-th-forward 'space arg (interactive-p)))

(defalias 'ar-backward-space-atpt 'ar-space-backward-atpt)
(defun ar-space-backward-atpt (&optional arg)
  "Moves backward over SPACE before point if any, does nothing otherwise.
Returns beginning position of SPACE "
  (interactive "p")
  (ar-th-backward 'space arg (interactive-p)))

(defalias 'ar-transpose-space-atpt 'ar-space-transpose-atpt)
(defun ar-space-transpose-atpt (&optional arg)
  "Transposes SPACE with SPACE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'space arg (interactive-p)))

(defalias 'ar-sort-space-atpt 'ar-space-sort-atpt)
(defun ar-space-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts spaces in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'space reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-space-atpt 'ar-space-check-atpt)
(defun ar-space-check-atpt ()
  "Return t if a SPACE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-space-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-space-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-upper-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around upper.
  Returns blok or nil if no UPPER at cursor-position. "
  (interactive "*")
  (ar-th-blok 'upper nil (interactive-p)))

(defun ar-comment-upper-atpt ()
  "Comments UPPER at point if any. "
  (interactive "*")
  (ar-th-comment 'upper nil (interactive-p)))

(defun ar-commatize-upper-atpt ()
  "Put a comma after UPPER at point if any. "
  (interactive "*")
  (ar-th-commatize 'upper nil (interactive-p)))

(defalias 'ar-mark-upper-atpt 'ar-upper-mark-atpt)
(defun ar-upper-mark-atpt ()
  "Marks UPPER at point if any. "
  (interactive)
  (ar-th-mark 'upper))

(defalias 'ar-hide-upper-atpt 'ar-upper-hide-atpt)
(defun ar-upper-hide-atpt ()
  "Hides UPPER at point. "
  (interactive)
  (ar-th-hide 'upper))

(defalias 'ar-show-upper-atpt 'ar-upper-show-atpt)
(defun ar-upper-show-atpt ()
  "Shows hidden UPPER at point. "
  (interactive)
  (ar-th-show 'upper))

(defalias 'ar-hide-show-upper-atpt 'ar-upper-hide-show-atpt)
(defun ar-upper-hide-show-atpt ()
  "Alternatively hides or shows UPPER at point. "
  (interactive)
  (ar-th-hide-show 'upper))

(defalias 'ar-highlight-upper-atpt-mode 'ar-upper-highlight-atpt-mode)

(defun ar-upper-highlight-atpt-mode ()
  "Toggles upper-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'upper nil (interactive-p)))

(defalias 'ar-kill-upper-atpt 'ar-upper-kill-atpt)
(defun ar-upper-kill-atpt ()
  "Kills UPPER at point if any. "
  (interactive "*")
  (ar-th-kill 'upper nil (interactive-p)))

(defalias 'ar-kill-backward-upper-atpt 'ar-upper-kill-backward-atpt)
(defun ar-upper-kill-backward-atpt ()
  "Kills UPPER at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'upper nil (interactive-p)))

(defalias 'ar-separate-upper-atpt 'ar-upper-separate-atpt)
(defun ar-upper-separate-atpt ()
  "Separates UPPER at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'upper nil (interactive-p)))

(defalias 'ar-triplequotedq-upper-atpt 'ar-upper-triplequotedq-atpt)
(defun ar-upper-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around upper. "
  (interactive "*")
  (ar-th-triplequotedq 'upper nil (interactive-p)))

(defalias 'ar-triplequotesq-upper-atpt 'ar-upper-triplequotesq-atpt)
(defun ar-upper-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around upper. "
  (interactive "*")
  (ar-th-triplequotesq 'upper nil (interactive-p)))

(defalias 'ar-forward-upper-atpt 'ar-upper-forward-atpt)
(defun ar-upper-forward-atpt (&optional arg)
  "Moves forward over UPPER at point if any, does nothing otherwise.
Returns end position of UPPER "
  (interactive "p")
  (ar-th-forward 'upper arg (interactive-p)))

(defalias 'ar-backward-upper-atpt 'ar-upper-backward-atpt)
(defun ar-upper-backward-atpt (&optional arg)
  "Moves backward over UPPER before point if any, does nothing otherwise.
Returns beginning position of UPPER "
  (interactive "p")
  (ar-th-backward 'upper arg (interactive-p)))

(defalias 'ar-transpose-upper-atpt 'ar-upper-transpose-atpt)
(defun ar-upper-transpose-atpt (&optional arg)
  "Transposes UPPER with UPPER before point if any. "
  (interactive "*p")
  (ar-th-transpose 'upper arg (interactive-p)))

(defalias 'ar-sort-upper-atpt 'ar-upper-sort-atpt)
(defun ar-upper-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts uppers in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'upper reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-upper-atpt 'ar-upper-check-atpt)
(defun ar-upper-check-atpt ()
  "Return t if a UPPER at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-upper-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-upper-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-blok-xdigit-atpt ()
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around xdigit.
  Returns blok or nil if no XDIGIT at cursor-position. "
  (interactive "*")
  (ar-th-blok 'xdigit nil (interactive-p)))

(defun ar-comment-xdigit-atpt ()
  "Comments XDIGIT at point if any. "
  (interactive "*")
  (ar-th-comment 'xdigit nil (interactive-p)))

(defun ar-commatize-xdigit-atpt ()
  "Put a comma after XDIGIT at point if any. "
  (interactive "*")
  (ar-th-commatize 'xdigit nil (interactive-p)))

(defalias 'ar-mark-xdigit-atpt 'ar-xdigit-mark-atpt)
(defun ar-xdigit-mark-atpt ()
  "Marks XDIGIT at point if any. "
  (interactive)
  (ar-th-mark 'xdigit))

(defalias 'ar-hide-xdigit-atpt 'ar-xdigit-hide-atpt)
(defun ar-xdigit-hide-atpt ()
  "Hides XDIGIT at point. "
  (interactive)
  (ar-th-hide 'xdigit))

(defalias 'ar-show-xdigit-atpt 'ar-xdigit-show-atpt)
(defun ar-xdigit-show-atpt ()
  "Shows hidden XDIGIT at point. "
  (interactive)
  (ar-th-show 'xdigit))

(defalias 'ar-hide-show-xdigit-atpt 'ar-xdigit-hide-show-atpt)
(defun ar-xdigit-hide-show-atpt ()
  "Alternatively hides or shows XDIGIT at point. "
  (interactive)
  (ar-th-hide-show 'xdigit))

(defalias 'ar-highlight-xdigit-atpt-mode 'ar-xdigit-highlight-atpt-mode)

(defun ar-xdigit-highlight-atpt-mode ()
  "Toggles xdigit-highlight-atpt-mode "
  (interactive)
  (ar-th-highlight 'xdigit nil (interactive-p)))

(defalias 'ar-kill-xdigit-atpt 'ar-xdigit-kill-atpt)
(defun ar-xdigit-kill-atpt ()
  "Kills XDIGIT at point if any. "
  (interactive "*")
  (ar-th-kill 'xdigit nil (interactive-p)))

(defalias 'ar-kill-backward-xdigit-atpt 'ar-xdigit-kill-backward-atpt)
(defun ar-xdigit-kill-backward-atpt ()
  "Kills XDIGIT at point if any. "
  (interactive "*")
  (ar-th-kill-backward 'xdigit nil (interactive-p)))

(defalias 'ar-separate-xdigit-atpt 'ar-xdigit-separate-atpt)
(defun ar-xdigit-separate-atpt ()
  "Separates XDIGIT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*")
  (ar-th-separate 'xdigit nil (interactive-p)))

(defalias 'ar-triplequotedq-xdigit-atpt 'ar-xdigit-triplequotedq-atpt)
(defun ar-xdigit-triplequotedq-atpt ()
  "Put triplequotes composed of doublequotes around xdigit. "
  (interactive "*")
  (ar-th-triplequotedq 'xdigit nil (interactive-p)))

(defalias 'ar-triplequotesq-xdigit-atpt 'ar-xdigit-triplequotesq-atpt)
(defun ar-xdigit-triplequotesq-atpt ()
  "Put triplequotes composed of singlequotes around xdigit. "
  (interactive "*")
  (ar-th-triplequotesq 'xdigit nil (interactive-p)))

(defalias 'ar-forward-xdigit-atpt 'ar-xdigit-forward-atpt)
(defun ar-xdigit-forward-atpt (&optional arg)
  "Moves forward over XDIGIT at point if any, does nothing otherwise.
Returns end position of XDIGIT "
  (interactive "p")
  (ar-th-forward 'xdigit arg (interactive-p)))

(defalias 'ar-backward-xdigit-atpt 'ar-xdigit-backward-atpt)
(defun ar-xdigit-backward-atpt (&optional arg)
  "Moves backward over XDIGIT before point if any, does nothing otherwise.
Returns beginning position of XDIGIT "
  (interactive "p")
  (ar-th-backward 'xdigit arg (interactive-p)))

(defalias 'ar-transpose-xdigit-atpt 'ar-xdigit-transpose-atpt)
(defun ar-xdigit-transpose-atpt (&optional arg)
  "Transposes XDIGIT with XDIGIT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'xdigit arg (interactive-p)))

(defalias 'ar-sort-xdigit-atpt 'ar-xdigit-sort-atpt)
(defun ar-xdigit-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts xdigits in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'xdigit reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-xdigit-atpt 'ar-xdigit-check-atpt)
(defun ar-xdigit-check-atpt ()
  "Return t if a XDIGIT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-xdigit-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-xdigit-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))
;; ar-thing-at-point-utils-nodelim-einzeln: end

;; ar-thing-at-point-utils-delimited-intern: 'list start

(defun ar-list-atpt (&optional arg no-delimiters)
  "Returns list at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'list arg no-delimiters (interactive-p)))

(defun ar-bounds-of-list-atpt (&optional no-delimiters)
  "Returns a list, borders of list if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'list no-delimiters (interactive-p)))

(defun ar-list-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position list at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'list no-delimiters (interactive-p)))

(defun ar-list-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of list at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'list no-delimiters (interactive-p)))

(defun ar-beginning-of-list-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class list at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'list no-delimiters (interactive-p)))

(defun ar-end-of-list-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class list at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'list no-delimiters (interactive-p)))

(defun ar-length-of-list-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class list at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'list no-delimiters (interactive-p)))

(defun ar-copy-list-atpt (&optional no-delimiters)
  "Returns a copy of list at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'list no-delimiters (interactive-p)))

(defun ar-delete-list-atpt (&optional arg)
  "Deletes list at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'list arg (interactive-p)))

(defun ar-delete-list-in-region (beg end)
  "Deletes list at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'list beg end (interactive-p)))

(defun ar-blok-list-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around list.

If region is active, do that for all elements \"list\" in region.
  Returns blok or nil if no list at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'list no-delimiters (interactive-p)))

(defun ar-doublebackslash-list-atpt (&optional no-delimiters)
  "Puts doubled backslashes around list at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'list no-delimiters (interactive-p)))

(defun ar-doubleslash-list-atpt (&optional no-delimiters)
  "Puts doubled slashes around list at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'list no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-list-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around list at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'list no-delimiters (interactive-p)))

;; (defun ar-slashparen-list-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around list at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'list no-delimiters (interactive-p)))

(defun ar-comment-list-atpt (&optional no-delimiters)
  "Comments list at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'list no-delimiters (interactive-p)))

(defun ar-commatize-list-atpt (&optional no-delimiters)
  "Put a comma after list at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'list no-delimiters (interactive-p)))

(defun ar-mark-list-atpt (&optional no-delimiters)
  "Marks list at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'list))

(defun ar-hide-list-atpt ()
  "Hides list at point. "
  (interactive)
  (ar-th-hide 'list))

(defun ar-show-list-atpt ()
  "Shows hidden list at point. "
  (interactive)
  (ar-th-show 'list))

(defun ar-hide-show-list-atpt ()
  "Alternatively hides or shows list at point. "
  (interactive)
  (ar-th-hide-show 'list))

(defun ar-highlight-list-atpt-mode (&optional no-delimiters)
  "Toggles list-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'list no-delimiters (interactive-p)))

(defun ar-kill-list-atpt (&optional no-delimiters)
  "Kills list at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'list no-delimiters (interactive-p)))

(defun ar-kill-backward-list-atpt (&optional no-delimiters)
  "Kills list at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'list no-delimiters (interactive-p)))

(defun ar-separate-list-atpt (&optional no-delimiters)
  "Separates list at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'list no-delimiters (interactive-p)))

(defun ar-forward-list-atpt (&optional arg)
  "Moves forward over list at point if any, does nothing otherwise.
Returns end position of list "
  (interactive "p")
  (ar-th-forward 'list arg (interactive-p)))

(defun ar-backward-list-atpt (&optional arg)
  "Moves backward over list before point if any, does nothing otherwise.
Returns beginning position of list "
  (interactive "p")
  (ar-th-backward 'list arg (interactive-p)))

(defun ar-transpose-list-atpt (&optional arg)
  "Transposes list with list before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'list arg (interactive-p)))

(defun ar-sort-list-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lists in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'list reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-list-atpt ()
  "Return t if a list at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-list-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-list-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimited-intern: 'list end

;; ar-thing-at-point-utils-delimited-intern: ar-unpaired-delimited-passiv-raw start

(defun ar-backslashed-atpt (&optional arg no-delimiters)
  "Returns backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'backslashed arg no-delimiters (interactive-p)))

(defun ar-bounds-of-backslashed-atpt (&optional no-delimiters)
  "Returns a list, borders of backslashed if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'backslashed no-delimiters (interactive-p)))

(defun ar-backslashed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'backslashed no-delimiters (interactive-p)))

(defun ar-backslashed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'backslashed no-delimiters (interactive-p)))

(defun ar-beginning-of-backslashed-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'backslashed no-delimiters (interactive-p)))

(defun ar-end-of-backslashed-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'backslashed no-delimiters (interactive-p)))

(defun ar-length-of-backslashed-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'backslashed no-delimiters (interactive-p)))

(defun ar-copy-backslashed-atpt (&optional no-delimiters)
  "Returns a copy of backslashed at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'backslashed no-delimiters (interactive-p)))

(defun ar-delete-backslashed-atpt (&optional arg)
  "Deletes backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'backslashed arg (interactive-p)))

(defun ar-delete-backslashed-in-region (beg end)
  "Deletes backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'backslashed beg end (interactive-p)))

(defun ar-blok-backslashed-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around backslashed.

If region is active, do that for all elements \"backslashed\" in region.
  Returns blok or nil if no backslashed at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'backslashed no-delimiters (interactive-p)))

(defun ar-doublebackslash-backslashed-atpt (&optional no-delimiters)
  "Puts doubled backslashes around backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'backslashed no-delimiters (interactive-p)))

(defun ar-doubleslash-backslashed-atpt (&optional no-delimiters)
  "Puts doubled slashes around backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'backslashed no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-backslashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around backslashed at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'backslashed no-delimiters (interactive-p)))

;; (defun ar-slashparen-backslashed-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around backslashed at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'backslashed no-delimiters (interactive-p)))

(defun ar-comment-backslashed-atpt (&optional no-delimiters)
  "Comments backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'backslashed no-delimiters (interactive-p)))

(defun ar-commatize-backslashed-atpt (&optional no-delimiters)
  "Put a comma after backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'backslashed no-delimiters (interactive-p)))

(defun ar-mark-backslashed-atpt (&optional no-delimiters)
  "Marks backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'backslashed))

(defun ar-hide-backslashed-atpt ()
  "Hides backslashed at point. "
  (interactive)
  (ar-th-hide 'backslashed))

(defun ar-show-backslashed-atpt ()
  "Shows hidden backslashed at point. "
  (interactive)
  (ar-th-show 'backslashed))

(defun ar-hide-show-backslashed-atpt ()
  "Alternatively hides or shows backslashed at point. "
  (interactive)
  (ar-th-hide-show 'backslashed))

(defun ar-highlight-backslashed-atpt-mode (&optional no-delimiters)
  "Toggles backslashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'backslashed no-delimiters (interactive-p)))

(defun ar-kill-backslashed-atpt (&optional no-delimiters)
  "Kills backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'backslashed no-delimiters (interactive-p)))

(defun ar-kill-backward-backslashed-atpt (&optional no-delimiters)
  "Kills backslashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'backslashed no-delimiters (interactive-p)))

(defun ar-separate-backslashed-atpt (&optional no-delimiters)
  "Separates backslashed at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'backslashed no-delimiters (interactive-p)))

(defun ar-forward-backslashed-atpt (&optional arg)
  "Moves forward over backslashed at point if any, does nothing otherwise.
Returns end position of backslashed "
  (interactive "p")
  (ar-th-forward 'backslashed arg (interactive-p)))

(defun ar-backward-backslashed-atpt (&optional arg)
  "Moves backward over backslashed before point if any, does nothing otherwise.
Returns beginning position of backslashed "
  (interactive "p")
  (ar-th-backward 'backslashed arg (interactive-p)))

(defun ar-transpose-backslashed-atpt (&optional arg)
  "Transposes backslashed with backslashed before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'backslashed arg (interactive-p)))

(defun ar-sort-backslashed-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts backslasheds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'backslashed reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-backslashed-atpt ()
  "Return t if a backslashed at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-backslashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-backslashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-backticked-atpt (&optional arg no-delimiters)
  "Returns backticked at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'backticked arg no-delimiters (interactive-p)))

(defun ar-bounds-of-backticked-atpt (&optional no-delimiters)
  "Returns a list, borders of backticked if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'backticked no-delimiters (interactive-p)))

(defun ar-backticked-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position backticked at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'backticked no-delimiters (interactive-p)))

(defun ar-backticked-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of backticked at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'backticked no-delimiters (interactive-p)))

(defun ar-beginning-of-backticked-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class backticked at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'backticked no-delimiters (interactive-p)))

(defun ar-end-of-backticked-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class backticked at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'backticked no-delimiters (interactive-p)))

(defun ar-length-of-backticked-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class backticked at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'backticked no-delimiters (interactive-p)))

(defun ar-copy-backticked-atpt (&optional no-delimiters)
  "Returns a copy of backticked at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'backticked no-delimiters (interactive-p)))

(defun ar-delete-backticked-atpt (&optional arg)
  "Deletes backticked at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'backticked arg (interactive-p)))

(defun ar-delete-backticked-in-region (beg end)
  "Deletes backticked at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'backticked beg end (interactive-p)))

(defun ar-blok-backticked-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around backticked.

If region is active, do that for all elements \"backticked\" in region.
  Returns blok or nil if no backticked at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'backticked no-delimiters (interactive-p)))

(defun ar-doublebackslash-backticked-atpt (&optional no-delimiters)
  "Puts doubled backslashes around backticked at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'backticked no-delimiters (interactive-p)))

(defun ar-doubleslash-backticked-atpt (&optional no-delimiters)
  "Puts doubled slashes around backticked at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'backticked no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-backticked-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around backticked at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'backticked no-delimiters (interactive-p)))

;; (defun ar-slashparen-backticked-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around backticked at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'backticked no-delimiters (interactive-p)))

(defun ar-comment-backticked-atpt (&optional no-delimiters)
  "Comments backticked at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'backticked no-delimiters (interactive-p)))

(defun ar-commatize-backticked-atpt (&optional no-delimiters)
  "Put a comma after backticked at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'backticked no-delimiters (interactive-p)))

(defun ar-mark-backticked-atpt (&optional no-delimiters)
  "Marks backticked at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'backticked))

(defun ar-hide-backticked-atpt ()
  "Hides backticked at point. "
  (interactive)
  (ar-th-hide 'backticked))

(defun ar-show-backticked-atpt ()
  "Shows hidden backticked at point. "
  (interactive)
  (ar-th-show 'backticked))

(defun ar-hide-show-backticked-atpt ()
  "Alternatively hides or shows backticked at point. "
  (interactive)
  (ar-th-hide-show 'backticked))

(defun ar-highlight-backticked-atpt-mode (&optional no-delimiters)
  "Toggles backticked-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'backticked no-delimiters (interactive-p)))

(defun ar-kill-backticked-atpt (&optional no-delimiters)
  "Kills backticked at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'backticked no-delimiters (interactive-p)))

(defun ar-kill-backward-backticked-atpt (&optional no-delimiters)
  "Kills backticked at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'backticked no-delimiters (interactive-p)))

(defun ar-separate-backticked-atpt (&optional no-delimiters)
  "Separates backticked at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'backticked no-delimiters (interactive-p)))

(defun ar-forward-backticked-atpt (&optional arg)
  "Moves forward over backticked at point if any, does nothing otherwise.
Returns end position of backticked "
  (interactive "p")
  (ar-th-forward 'backticked arg (interactive-p)))

(defun ar-backward-backticked-atpt (&optional arg)
  "Moves backward over backticked before point if any, does nothing otherwise.
Returns beginning position of backticked "
  (interactive "p")
  (ar-th-backward 'backticked arg (interactive-p)))

(defun ar-transpose-backticked-atpt (&optional arg)
  "Transposes backticked with backticked before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'backticked arg (interactive-p)))

(defun ar-sort-backticked-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts backtickeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'backticked reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-backticked-atpt ()
  "Return t if a backticked at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-backticked-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-backticked-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-coloned-atpt (&optional arg no-delimiters)
  "Returns coloned at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'coloned arg no-delimiters (interactive-p)))

(defun ar-bounds-of-coloned-atpt (&optional no-delimiters)
  "Returns a list, borders of coloned if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'coloned no-delimiters (interactive-p)))

(defun ar-coloned-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position coloned at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'coloned no-delimiters (interactive-p)))

(defun ar-coloned-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of coloned at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'coloned no-delimiters (interactive-p)))

(defun ar-beginning-of-coloned-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class coloned at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'coloned no-delimiters (interactive-p)))

(defun ar-end-of-coloned-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class coloned at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'coloned no-delimiters (interactive-p)))

(defun ar-length-of-coloned-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class coloned at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'coloned no-delimiters (interactive-p)))

(defun ar-copy-coloned-atpt (&optional no-delimiters)
  "Returns a copy of coloned at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'coloned no-delimiters (interactive-p)))

(defun ar-delete-coloned-atpt (&optional arg)
  "Deletes coloned at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'coloned arg (interactive-p)))

(defun ar-delete-coloned-in-region (beg end)
  "Deletes coloned at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'coloned beg end (interactive-p)))

(defun ar-blok-coloned-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around coloned.

If region is active, do that for all elements \"coloned\" in region.
  Returns blok or nil if no coloned at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'coloned no-delimiters (interactive-p)))

(defun ar-doublebackslash-coloned-atpt (&optional no-delimiters)
  "Puts doubled backslashes around coloned at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'coloned no-delimiters (interactive-p)))

(defun ar-doubleslash-coloned-atpt (&optional no-delimiters)
  "Puts doubled slashes around coloned at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'coloned no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-coloned-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around coloned at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'coloned no-delimiters (interactive-p)))

;; (defun ar-slashparen-coloned-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around coloned at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'coloned no-delimiters (interactive-p)))

(defun ar-comment-coloned-atpt (&optional no-delimiters)
  "Comments coloned at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'coloned no-delimiters (interactive-p)))

(defun ar-commatize-coloned-atpt (&optional no-delimiters)
  "Put a comma after coloned at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'coloned no-delimiters (interactive-p)))

(defun ar-mark-coloned-atpt (&optional no-delimiters)
  "Marks coloned at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'coloned))

(defun ar-hide-coloned-atpt ()
  "Hides coloned at point. "
  (interactive)
  (ar-th-hide 'coloned))

(defun ar-show-coloned-atpt ()
  "Shows hidden coloned at point. "
  (interactive)
  (ar-th-show 'coloned))

(defun ar-hide-show-coloned-atpt ()
  "Alternatively hides or shows coloned at point. "
  (interactive)
  (ar-th-hide-show 'coloned))

(defun ar-highlight-coloned-atpt-mode (&optional no-delimiters)
  "Toggles coloned-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'coloned no-delimiters (interactive-p)))

(defun ar-kill-coloned-atpt (&optional no-delimiters)
  "Kills coloned at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'coloned no-delimiters (interactive-p)))

(defun ar-kill-backward-coloned-atpt (&optional no-delimiters)
  "Kills coloned at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'coloned no-delimiters (interactive-p)))

(defun ar-separate-coloned-atpt (&optional no-delimiters)
  "Separates coloned at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'coloned no-delimiters (interactive-p)))

(defun ar-forward-coloned-atpt (&optional arg)
  "Moves forward over coloned at point if any, does nothing otherwise.
Returns end position of coloned "
  (interactive "p")
  (ar-th-forward 'coloned arg (interactive-p)))

(defun ar-backward-coloned-atpt (&optional arg)
  "Moves backward over coloned before point if any, does nothing otherwise.
Returns beginning position of coloned "
  (interactive "p")
  (ar-th-backward 'coloned arg (interactive-p)))

(defun ar-transpose-coloned-atpt (&optional arg)
  "Transposes coloned with coloned before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'coloned arg (interactive-p)))

(defun ar-sort-coloned-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts coloneds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'coloned reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-coloned-atpt ()
  "Return t if a coloned at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-coloned-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-coloned-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-crossed-atpt (&optional arg no-delimiters)
  "Returns crossed at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'crossed arg no-delimiters (interactive-p)))

(defun ar-bounds-of-crossed-atpt (&optional no-delimiters)
  "Returns a list, borders of crossed if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'crossed no-delimiters (interactive-p)))

(defun ar-crossed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position crossed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'crossed no-delimiters (interactive-p)))

(defun ar-crossed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of crossed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'crossed no-delimiters (interactive-p)))

(defun ar-beginning-of-crossed-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class crossed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'crossed no-delimiters (interactive-p)))

(defun ar-end-of-crossed-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class crossed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'crossed no-delimiters (interactive-p)))

(defun ar-length-of-crossed-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class crossed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'crossed no-delimiters (interactive-p)))

(defun ar-copy-crossed-atpt (&optional no-delimiters)
  "Returns a copy of crossed at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'crossed no-delimiters (interactive-p)))

(defun ar-delete-crossed-atpt (&optional arg)
  "Deletes crossed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'crossed arg (interactive-p)))

(defun ar-delete-crossed-in-region (beg end)
  "Deletes crossed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'crossed beg end (interactive-p)))

(defun ar-blok-crossed-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around crossed.

If region is active, do that for all elements \"crossed\" in region.
  Returns blok or nil if no crossed at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'crossed no-delimiters (interactive-p)))

(defun ar-doublebackslash-crossed-atpt (&optional no-delimiters)
  "Puts doubled backslashes around crossed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'crossed no-delimiters (interactive-p)))

(defun ar-doubleslash-crossed-atpt (&optional no-delimiters)
  "Puts doubled slashes around crossed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'crossed no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-crossed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around crossed at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'crossed no-delimiters (interactive-p)))

;; (defun ar-slashparen-crossed-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around crossed at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'crossed no-delimiters (interactive-p)))

(defun ar-comment-crossed-atpt (&optional no-delimiters)
  "Comments crossed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'crossed no-delimiters (interactive-p)))

(defun ar-commatize-crossed-atpt (&optional no-delimiters)
  "Put a comma after crossed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'crossed no-delimiters (interactive-p)))

(defun ar-mark-crossed-atpt (&optional no-delimiters)
  "Marks crossed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'crossed))

(defun ar-hide-crossed-atpt ()
  "Hides crossed at point. "
  (interactive)
  (ar-th-hide 'crossed))

(defun ar-show-crossed-atpt ()
  "Shows hidden crossed at point. "
  (interactive)
  (ar-th-show 'crossed))

(defun ar-hide-show-crossed-atpt ()
  "Alternatively hides or shows crossed at point. "
  (interactive)
  (ar-th-hide-show 'crossed))

(defun ar-highlight-crossed-atpt-mode (&optional no-delimiters)
  "Toggles crossed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'crossed no-delimiters (interactive-p)))

(defun ar-kill-crossed-atpt (&optional no-delimiters)
  "Kills crossed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'crossed no-delimiters (interactive-p)))

(defun ar-kill-backward-crossed-atpt (&optional no-delimiters)
  "Kills crossed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'crossed no-delimiters (interactive-p)))

(defun ar-separate-crossed-atpt (&optional no-delimiters)
  "Separates crossed at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'crossed no-delimiters (interactive-p)))

(defun ar-forward-crossed-atpt (&optional arg)
  "Moves forward over crossed at point if any, does nothing otherwise.
Returns end position of crossed "
  (interactive "p")
  (ar-th-forward 'crossed arg (interactive-p)))

(defun ar-backward-crossed-atpt (&optional arg)
  "Moves backward over crossed before point if any, does nothing otherwise.
Returns beginning position of crossed "
  (interactive "p")
  (ar-th-backward 'crossed arg (interactive-p)))

(defun ar-transpose-crossed-atpt (&optional arg)
  "Transposes crossed with crossed before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'crossed arg (interactive-p)))

(defun ar-sort-crossed-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts crosseds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'crossed reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-crossed-atpt ()
  "Return t if a crossed at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-crossed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-crossed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-dollared-atpt (&optional arg no-delimiters)
  "Returns dollared at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'dollared arg no-delimiters (interactive-p)))

(defun ar-bounds-of-dollared-atpt (&optional no-delimiters)
  "Returns a list, borders of dollared if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'dollared no-delimiters (interactive-p)))

(defun ar-dollared-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position dollared at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'dollared no-delimiters (interactive-p)))

(defun ar-dollared-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of dollared at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'dollared no-delimiters (interactive-p)))

(defun ar-beginning-of-dollared-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class dollared at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'dollared no-delimiters (interactive-p)))

(defun ar-end-of-dollared-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class dollared at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'dollared no-delimiters (interactive-p)))

(defun ar-length-of-dollared-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class dollared at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'dollared no-delimiters (interactive-p)))

(defun ar-copy-dollared-atpt (&optional no-delimiters)
  "Returns a copy of dollared at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'dollared no-delimiters (interactive-p)))

(defun ar-delete-dollared-atpt (&optional arg)
  "Deletes dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'dollared arg (interactive-p)))

(defun ar-delete-dollared-in-region (beg end)
  "Deletes dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'dollared beg end (interactive-p)))

(defun ar-blok-dollared-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around dollared.

If region is active, do that for all elements \"dollared\" in region.
  Returns blok or nil if no dollared at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'dollared no-delimiters (interactive-p)))

(defun ar-doublebackslash-dollared-atpt (&optional no-delimiters)
  "Puts doubled backslashes around dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'dollared no-delimiters (interactive-p)))

(defun ar-doubleslash-dollared-atpt (&optional no-delimiters)
  "Puts doubled slashes around dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'dollared no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-dollared-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around dollared at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'dollared no-delimiters (interactive-p)))

;; (defun ar-slashparen-dollared-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around dollared at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'dollared no-delimiters (interactive-p)))

(defun ar-comment-dollared-atpt (&optional no-delimiters)
  "Comments dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'dollared no-delimiters (interactive-p)))

(defun ar-commatize-dollared-atpt (&optional no-delimiters)
  "Put a comma after dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'dollared no-delimiters (interactive-p)))

(defun ar-mark-dollared-atpt (&optional no-delimiters)
  "Marks dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'dollared))

(defun ar-hide-dollared-atpt ()
  "Hides dollared at point. "
  (interactive)
  (ar-th-hide 'dollared))

(defun ar-show-dollared-atpt ()
  "Shows hidden dollared at point. "
  (interactive)
  (ar-th-show 'dollared))

(defun ar-hide-show-dollared-atpt ()
  "Alternatively hides or shows dollared at point. "
  (interactive)
  (ar-th-hide-show 'dollared))

(defun ar-highlight-dollared-atpt-mode (&optional no-delimiters)
  "Toggles dollared-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'dollared no-delimiters (interactive-p)))

(defun ar-kill-dollared-atpt (&optional no-delimiters)
  "Kills dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'dollared no-delimiters (interactive-p)))

(defun ar-kill-backward-dollared-atpt (&optional no-delimiters)
  "Kills dollared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'dollared no-delimiters (interactive-p)))

(defun ar-separate-dollared-atpt (&optional no-delimiters)
  "Separates dollared at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'dollared no-delimiters (interactive-p)))

(defun ar-forward-dollared-atpt (&optional arg)
  "Moves forward over dollared at point if any, does nothing otherwise.
Returns end position of dollared "
  (interactive "p")
  (ar-th-forward 'dollared arg (interactive-p)))

(defun ar-backward-dollared-atpt (&optional arg)
  "Moves backward over dollared before point if any, does nothing otherwise.
Returns beginning position of dollared "
  (interactive "p")
  (ar-th-backward 'dollared arg (interactive-p)))

(defun ar-transpose-dollared-atpt (&optional arg)
  "Transposes dollared with dollared before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'dollared arg (interactive-p)))

(defun ar-sort-dollared-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts dollareds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'dollared reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-dollared-atpt ()
  "Return t if a dollared at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-dollared-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-dollared-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-doublequoted-atpt (&optional arg no-delimiters)
  "Returns doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'doublequoted arg no-delimiters (interactive-p)))

(defun ar-bounds-of-doublequoted-atpt (&optional no-delimiters)
  "Returns a list, borders of doublequoted if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'doublequoted no-delimiters (interactive-p)))

(defun ar-doublequoted-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'doublequoted no-delimiters (interactive-p)))

(defun ar-doublequoted-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'doublequoted no-delimiters (interactive-p)))

(defun ar-beginning-of-doublequoted-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'doublequoted no-delimiters (interactive-p)))

(defun ar-end-of-doublequoted-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'doublequoted no-delimiters (interactive-p)))

(defun ar-length-of-doublequoted-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'doublequoted no-delimiters (interactive-p)))

(defun ar-copy-doublequoted-atpt (&optional no-delimiters)
  "Returns a copy of doublequoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'doublequoted no-delimiters (interactive-p)))

(defun ar-delete-doublequoted-atpt (&optional arg)
  "Deletes doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'doublequoted arg (interactive-p)))

(defun ar-delete-doublequoted-in-region (beg end)
  "Deletes doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'doublequoted beg end (interactive-p)))

(defun ar-blok-doublequoted-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around doublequoted.

If region is active, do that for all elements \"doublequoted\" in region.
  Returns blok or nil if no doublequoted at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'doublequoted no-delimiters (interactive-p)))

(defun ar-doublebackslash-doublequoted-atpt (&optional no-delimiters)
  "Puts doubled backslashes around doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'doublequoted no-delimiters (interactive-p)))

(defun ar-doubleslash-doublequoted-atpt (&optional no-delimiters)
  "Puts doubled slashes around doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'doublequoted no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-doublequoted-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around doublequoted at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'doublequoted no-delimiters (interactive-p)))

;; (defun ar-slashparen-doublequoted-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around doublequoted at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'doublequoted no-delimiters (interactive-p)))

(defun ar-comment-doublequoted-atpt (&optional no-delimiters)
  "Comments doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'doublequoted no-delimiters (interactive-p)))

(defun ar-commatize-doublequoted-atpt (&optional no-delimiters)
  "Put a comma after doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'doublequoted no-delimiters (interactive-p)))

(defun ar-mark-doublequoted-atpt (&optional no-delimiters)
  "Marks doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'doublequoted))

(defun ar-hide-doublequoted-atpt ()
  "Hides doublequoted at point. "
  (interactive)
  (ar-th-hide 'doublequoted))

(defun ar-show-doublequoted-atpt ()
  "Shows hidden doublequoted at point. "
  (interactive)
  (ar-th-show 'doublequoted))

(defun ar-hide-show-doublequoted-atpt ()
  "Alternatively hides or shows doublequoted at point. "
  (interactive)
  (ar-th-hide-show 'doublequoted))

(defun ar-highlight-doublequoted-atpt-mode (&optional no-delimiters)
  "Toggles doublequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'doublequoted no-delimiters (interactive-p)))

(defun ar-kill-doublequoted-atpt (&optional no-delimiters)
  "Kills doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'doublequoted no-delimiters (interactive-p)))

(defun ar-kill-backward-doublequoted-atpt (&optional no-delimiters)
  "Kills doublequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'doublequoted no-delimiters (interactive-p)))

(defun ar-separate-doublequoted-atpt (&optional no-delimiters)
  "Separates doublequoted at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'doublequoted no-delimiters (interactive-p)))

(defun ar-forward-doublequoted-atpt (&optional arg)
  "Moves forward over doublequoted at point if any, does nothing otherwise.
Returns end position of doublequoted "
  (interactive "p")
  (ar-th-forward 'doublequoted arg (interactive-p)))

(defun ar-backward-doublequoted-atpt (&optional arg)
  "Moves backward over doublequoted before point if any, does nothing otherwise.
Returns beginning position of doublequoted "
  (interactive "p")
  (ar-th-backward 'doublequoted arg (interactive-p)))

(defun ar-transpose-doublequoted-atpt (&optional arg)
  "Transposes doublequoted with doublequoted before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'doublequoted arg (interactive-p)))

(defun ar-sort-doublequoted-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts doublequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'doublequoted reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-doublequoted-atpt ()
  "Return t if a doublequoted at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-doublequoted-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-doublequoted-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-equalized-atpt (&optional arg no-delimiters)
  "Returns equalized at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'equalized arg no-delimiters (interactive-p)))

(defun ar-bounds-of-equalized-atpt (&optional no-delimiters)
  "Returns a list, borders of equalized if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'equalized no-delimiters (interactive-p)))

(defun ar-equalized-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position equalized at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'equalized no-delimiters (interactive-p)))

(defun ar-equalized-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of equalized at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'equalized no-delimiters (interactive-p)))

(defun ar-beginning-of-equalized-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class equalized at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'equalized no-delimiters (interactive-p)))

(defun ar-end-of-equalized-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class equalized at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'equalized no-delimiters (interactive-p)))

(defun ar-length-of-equalized-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class equalized at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'equalized no-delimiters (interactive-p)))

(defun ar-copy-equalized-atpt (&optional no-delimiters)
  "Returns a copy of equalized at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'equalized no-delimiters (interactive-p)))

(defun ar-delete-equalized-atpt (&optional arg)
  "Deletes equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'equalized arg (interactive-p)))

(defun ar-delete-equalized-in-region (beg end)
  "Deletes equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'equalized beg end (interactive-p)))

(defun ar-blok-equalized-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around equalized.

If region is active, do that for all elements \"equalized\" in region.
  Returns blok or nil if no equalized at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'equalized no-delimiters (interactive-p)))

(defun ar-doublebackslash-equalized-atpt (&optional no-delimiters)
  "Puts doubled backslashes around equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'equalized no-delimiters (interactive-p)))

(defun ar-doubleslash-equalized-atpt (&optional no-delimiters)
  "Puts doubled slashes around equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'equalized no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-equalized-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around equalized at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'equalized no-delimiters (interactive-p)))

;; (defun ar-slashparen-equalized-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around equalized at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'equalized no-delimiters (interactive-p)))

(defun ar-comment-equalized-atpt (&optional no-delimiters)
  "Comments equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'equalized no-delimiters (interactive-p)))

(defun ar-commatize-equalized-atpt (&optional no-delimiters)
  "Put a comma after equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'equalized no-delimiters (interactive-p)))

(defun ar-mark-equalized-atpt (&optional no-delimiters)
  "Marks equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'equalized))

(defun ar-hide-equalized-atpt ()
  "Hides equalized at point. "
  (interactive)
  (ar-th-hide 'equalized))

(defun ar-show-equalized-atpt ()
  "Shows hidden equalized at point. "
  (interactive)
  (ar-th-show 'equalized))

(defun ar-hide-show-equalized-atpt ()
  "Alternatively hides or shows equalized at point. "
  (interactive)
  (ar-th-hide-show 'equalized))

(defun ar-highlight-equalized-atpt-mode (&optional no-delimiters)
  "Toggles equalized-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'equalized no-delimiters (interactive-p)))

(defun ar-kill-equalized-atpt (&optional no-delimiters)
  "Kills equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'equalized no-delimiters (interactive-p)))

(defun ar-kill-backward-equalized-atpt (&optional no-delimiters)
  "Kills equalized at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'equalized no-delimiters (interactive-p)))

(defun ar-separate-equalized-atpt (&optional no-delimiters)
  "Separates equalized at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'equalized no-delimiters (interactive-p)))

(defun ar-forward-equalized-atpt (&optional arg)
  "Moves forward over equalized at point if any, does nothing otherwise.
Returns end position of equalized "
  (interactive "p")
  (ar-th-forward 'equalized arg (interactive-p)))

(defun ar-backward-equalized-atpt (&optional arg)
  "Moves backward over equalized before point if any, does nothing otherwise.
Returns beginning position of equalized "
  (interactive "p")
  (ar-th-backward 'equalized arg (interactive-p)))

(defun ar-transpose-equalized-atpt (&optional arg)
  "Transposes equalized with equalized before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'equalized arg (interactive-p)))

(defun ar-sort-equalized-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts equalizeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'equalized reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-equalized-atpt ()
  "Return t if a equalized at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-equalized-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-equalized-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-hashed-atpt (&optional arg no-delimiters)
  "Returns hashed at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'hashed arg no-delimiters (interactive-p)))

(defun ar-bounds-of-hashed-atpt (&optional no-delimiters)
  "Returns a list, borders of hashed if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'hashed no-delimiters (interactive-p)))

(defun ar-hashed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position hashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'hashed no-delimiters (interactive-p)))

(defun ar-hashed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of hashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'hashed no-delimiters (interactive-p)))

(defun ar-beginning-of-hashed-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class hashed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'hashed no-delimiters (interactive-p)))

(defun ar-end-of-hashed-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class hashed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'hashed no-delimiters (interactive-p)))

(defun ar-length-of-hashed-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class hashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'hashed no-delimiters (interactive-p)))

(defun ar-copy-hashed-atpt (&optional no-delimiters)
  "Returns a copy of hashed at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'hashed no-delimiters (interactive-p)))

(defun ar-delete-hashed-atpt (&optional arg)
  "Deletes hashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'hashed arg (interactive-p)))

(defun ar-delete-hashed-in-region (beg end)
  "Deletes hashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'hashed beg end (interactive-p)))

(defun ar-blok-hashed-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around hashed.

If region is active, do that for all elements \"hashed\" in region.
  Returns blok or nil if no hashed at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'hashed no-delimiters (interactive-p)))

(defun ar-doublebackslash-hashed-atpt (&optional no-delimiters)
  "Puts doubled backslashes around hashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'hashed no-delimiters (interactive-p)))

(defun ar-doubleslash-hashed-atpt (&optional no-delimiters)
  "Puts doubled slashes around hashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'hashed no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-hashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around hashed at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'hashed no-delimiters (interactive-p)))

;; (defun ar-slashparen-hashed-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around hashed at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'hashed no-delimiters (interactive-p)))

(defun ar-comment-hashed-atpt (&optional no-delimiters)
  "Comments hashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'hashed no-delimiters (interactive-p)))

(defun ar-commatize-hashed-atpt (&optional no-delimiters)
  "Put a comma after hashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'hashed no-delimiters (interactive-p)))

(defun ar-mark-hashed-atpt (&optional no-delimiters)
  "Marks hashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'hashed))

(defun ar-hide-hashed-atpt ()
  "Hides hashed at point. "
  (interactive)
  (ar-th-hide 'hashed))

(defun ar-show-hashed-atpt ()
  "Shows hidden hashed at point. "
  (interactive)
  (ar-th-show 'hashed))

(defun ar-hide-show-hashed-atpt ()
  "Alternatively hides or shows hashed at point. "
  (interactive)
  (ar-th-hide-show 'hashed))

(defun ar-highlight-hashed-atpt-mode (&optional no-delimiters)
  "Toggles hashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'hashed no-delimiters (interactive-p)))

(defun ar-kill-hashed-atpt (&optional no-delimiters)
  "Kills hashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'hashed no-delimiters (interactive-p)))

(defun ar-kill-backward-hashed-atpt (&optional no-delimiters)
  "Kills hashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'hashed no-delimiters (interactive-p)))

(defun ar-separate-hashed-atpt (&optional no-delimiters)
  "Separates hashed at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'hashed no-delimiters (interactive-p)))

(defun ar-forward-hashed-atpt (&optional arg)
  "Moves forward over hashed at point if any, does nothing otherwise.
Returns end position of hashed "
  (interactive "p")
  (ar-th-forward 'hashed arg (interactive-p)))

(defun ar-backward-hashed-atpt (&optional arg)
  "Moves backward over hashed before point if any, does nothing otherwise.
Returns beginning position of hashed "
  (interactive "p")
  (ar-th-backward 'hashed arg (interactive-p)))

(defun ar-transpose-hashed-atpt (&optional arg)
  "Transposes hashed with hashed before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'hashed arg (interactive-p)))

(defun ar-sort-hashed-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts hasheds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'hashed reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-hashed-atpt ()
  "Return t if a hashed at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-hashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-hashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-hyphened-atpt (&optional arg no-delimiters)
  "Returns hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'hyphened arg no-delimiters (interactive-p)))

(defun ar-bounds-of-hyphened-atpt (&optional no-delimiters)
  "Returns a list, borders of hyphened if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'hyphened no-delimiters (interactive-p)))

(defun ar-hyphened-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'hyphened no-delimiters (interactive-p)))

(defun ar-hyphened-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'hyphened no-delimiters (interactive-p)))

(defun ar-beginning-of-hyphened-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'hyphened no-delimiters (interactive-p)))

(defun ar-end-of-hyphened-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'hyphened no-delimiters (interactive-p)))

(defun ar-length-of-hyphened-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'hyphened no-delimiters (interactive-p)))

(defun ar-copy-hyphened-atpt (&optional no-delimiters)
  "Returns a copy of hyphened at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'hyphened no-delimiters (interactive-p)))

(defun ar-delete-hyphened-atpt (&optional arg)
  "Deletes hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'hyphened arg (interactive-p)))

(defun ar-delete-hyphened-in-region (beg end)
  "Deletes hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'hyphened beg end (interactive-p)))

(defun ar-blok-hyphened-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around hyphened.

If region is active, do that for all elements \"hyphened\" in region.
  Returns blok or nil if no hyphened at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'hyphened no-delimiters (interactive-p)))

(defun ar-doublebackslash-hyphened-atpt (&optional no-delimiters)
  "Puts doubled backslashes around hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'hyphened no-delimiters (interactive-p)))

(defun ar-doubleslash-hyphened-atpt (&optional no-delimiters)
  "Puts doubled slashes around hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'hyphened no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-hyphened-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around hyphened at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'hyphened no-delimiters (interactive-p)))

;; (defun ar-slashparen-hyphened-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around hyphened at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'hyphened no-delimiters (interactive-p)))

(defun ar-comment-hyphened-atpt (&optional no-delimiters)
  "Comments hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'hyphened no-delimiters (interactive-p)))

(defun ar-commatize-hyphened-atpt (&optional no-delimiters)
  "Put a comma after hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'hyphened no-delimiters (interactive-p)))

(defun ar-mark-hyphened-atpt (&optional no-delimiters)
  "Marks hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'hyphened))

(defun ar-hide-hyphened-atpt ()
  "Hides hyphened at point. "
  (interactive)
  (ar-th-hide 'hyphened))

(defun ar-show-hyphened-atpt ()
  "Shows hidden hyphened at point. "
  (interactive)
  (ar-th-show 'hyphened))

(defun ar-hide-show-hyphened-atpt ()
  "Alternatively hides or shows hyphened at point. "
  (interactive)
  (ar-th-hide-show 'hyphened))

(defun ar-highlight-hyphened-atpt-mode (&optional no-delimiters)
  "Toggles hyphened-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'hyphened no-delimiters (interactive-p)))

(defun ar-kill-hyphened-atpt (&optional no-delimiters)
  "Kills hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'hyphened no-delimiters (interactive-p)))

(defun ar-kill-backward-hyphened-atpt (&optional no-delimiters)
  "Kills hyphened at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'hyphened no-delimiters (interactive-p)))

(defun ar-separate-hyphened-atpt (&optional no-delimiters)
  "Separates hyphened at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'hyphened no-delimiters (interactive-p)))

(defun ar-forward-hyphened-atpt (&optional arg)
  "Moves forward over hyphened at point if any, does nothing otherwise.
Returns end position of hyphened "
  (interactive "p")
  (ar-th-forward 'hyphened arg (interactive-p)))

(defun ar-backward-hyphened-atpt (&optional arg)
  "Moves backward over hyphened before point if any, does nothing otherwise.
Returns beginning position of hyphened "
  (interactive "p")
  (ar-th-backward 'hyphened arg (interactive-p)))

(defun ar-transpose-hyphened-atpt (&optional arg)
  "Transposes hyphened with hyphened before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'hyphened arg (interactive-p)))

(defun ar-sort-hyphened-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts hypheneds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'hyphened reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-hyphened-atpt ()
  "Return t if a hyphened at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-hyphened-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-hyphened-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-singlequoted-atpt (&optional arg no-delimiters)
  "Returns singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'singlequoted arg no-delimiters (interactive-p)))

(defun ar-bounds-of-singlequoted-atpt (&optional no-delimiters)
  "Returns a list, borders of singlequoted if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'singlequoted no-delimiters (interactive-p)))

(defun ar-singlequoted-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'singlequoted no-delimiters (interactive-p)))

(defun ar-singlequoted-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'singlequoted no-delimiters (interactive-p)))

(defun ar-beginning-of-singlequoted-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'singlequoted no-delimiters (interactive-p)))

(defun ar-end-of-singlequoted-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'singlequoted no-delimiters (interactive-p)))

(defun ar-length-of-singlequoted-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'singlequoted no-delimiters (interactive-p)))

(defun ar-copy-singlequoted-atpt (&optional no-delimiters)
  "Returns a copy of singlequoted at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'singlequoted no-delimiters (interactive-p)))

(defun ar-delete-singlequoted-atpt (&optional arg)
  "Deletes singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'singlequoted arg (interactive-p)))

(defun ar-delete-singlequoted-in-region (beg end)
  "Deletes singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'singlequoted beg end (interactive-p)))

(defun ar-blok-singlequoted-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around singlequoted.

If region is active, do that for all elements \"singlequoted\" in region.
  Returns blok or nil if no singlequoted at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'singlequoted no-delimiters (interactive-p)))

(defun ar-doublebackslash-singlequoted-atpt (&optional no-delimiters)
  "Puts doubled backslashes around singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'singlequoted no-delimiters (interactive-p)))

(defun ar-doubleslash-singlequoted-atpt (&optional no-delimiters)
  "Puts doubled slashes around singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'singlequoted no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-singlequoted-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around singlequoted at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'singlequoted no-delimiters (interactive-p)))

;; (defun ar-slashparen-singlequoted-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around singlequoted at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'singlequoted no-delimiters (interactive-p)))

(defun ar-comment-singlequoted-atpt (&optional no-delimiters)
  "Comments singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'singlequoted no-delimiters (interactive-p)))

(defun ar-commatize-singlequoted-atpt (&optional no-delimiters)
  "Put a comma after singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'singlequoted no-delimiters (interactive-p)))

(defun ar-mark-singlequoted-atpt (&optional no-delimiters)
  "Marks singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'singlequoted))

(defun ar-hide-singlequoted-atpt ()
  "Hides singlequoted at point. "
  (interactive)
  (ar-th-hide 'singlequoted))

(defun ar-show-singlequoted-atpt ()
  "Shows hidden singlequoted at point. "
  (interactive)
  (ar-th-show 'singlequoted))

(defun ar-hide-show-singlequoted-atpt ()
  "Alternatively hides or shows singlequoted at point. "
  (interactive)
  (ar-th-hide-show 'singlequoted))

(defun ar-highlight-singlequoted-atpt-mode (&optional no-delimiters)
  "Toggles singlequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'singlequoted no-delimiters (interactive-p)))

(defun ar-kill-singlequoted-atpt (&optional no-delimiters)
  "Kills singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'singlequoted no-delimiters (interactive-p)))

(defun ar-kill-backward-singlequoted-atpt (&optional no-delimiters)
  "Kills singlequoted at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'singlequoted no-delimiters (interactive-p)))

(defun ar-separate-singlequoted-atpt (&optional no-delimiters)
  "Separates singlequoted at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'singlequoted no-delimiters (interactive-p)))

(defun ar-forward-singlequoted-atpt (&optional arg)
  "Moves forward over singlequoted at point if any, does nothing otherwise.
Returns end position of singlequoted "
  (interactive "p")
  (ar-th-forward 'singlequoted arg (interactive-p)))

(defun ar-backward-singlequoted-atpt (&optional arg)
  "Moves backward over singlequoted before point if any, does nothing otherwise.
Returns beginning position of singlequoted "
  (interactive "p")
  (ar-th-backward 'singlequoted arg (interactive-p)))

(defun ar-transpose-singlequoted-atpt (&optional arg)
  "Transposes singlequoted with singlequoted before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'singlequoted arg (interactive-p)))

(defun ar-sort-singlequoted-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts singlequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'singlequoted reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-singlequoted-atpt ()
  "Return t if a singlequoted at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-singlequoted-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-singlequoted-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-slashed-atpt (&optional arg no-delimiters)
  "Returns slashed at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'slashed arg no-delimiters (interactive-p)))

(defun ar-bounds-of-slashed-atpt (&optional no-delimiters)
  "Returns a list, borders of slashed if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'slashed no-delimiters (interactive-p)))

(defun ar-slashed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position slashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'slashed no-delimiters (interactive-p)))

(defun ar-slashed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of slashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'slashed no-delimiters (interactive-p)))

(defun ar-beginning-of-slashed-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class slashed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'slashed no-delimiters (interactive-p)))

(defun ar-end-of-slashed-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class slashed at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'slashed no-delimiters (interactive-p)))

(defun ar-length-of-slashed-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class slashed at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'slashed no-delimiters (interactive-p)))

(defun ar-copy-slashed-atpt (&optional no-delimiters)
  "Returns a copy of slashed at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'slashed no-delimiters (interactive-p)))

(defun ar-delete-slashed-atpt (&optional arg)
  "Deletes slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'slashed arg (interactive-p)))

(defun ar-delete-slashed-in-region (beg end)
  "Deletes slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'slashed beg end (interactive-p)))

(defun ar-blok-slashed-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around slashed.

If region is active, do that for all elements \"slashed\" in region.
  Returns blok or nil if no slashed at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'slashed no-delimiters (interactive-p)))

(defun ar-doublebackslash-slashed-atpt (&optional no-delimiters)
  "Puts doubled backslashes around slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'slashed no-delimiters (interactive-p)))

(defun ar-doubleslash-slashed-atpt (&optional no-delimiters)
  "Puts doubled slashes around slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'slashed no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-slashed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around slashed at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'slashed no-delimiters (interactive-p)))

;; (defun ar-slashparen-slashed-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around slashed at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'slashed no-delimiters (interactive-p)))

(defun ar-comment-slashed-atpt (&optional no-delimiters)
  "Comments slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'slashed no-delimiters (interactive-p)))

(defun ar-commatize-slashed-atpt (&optional no-delimiters)
  "Put a comma after slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'slashed no-delimiters (interactive-p)))

(defun ar-mark-slashed-atpt (&optional no-delimiters)
  "Marks slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'slashed))

(defun ar-hide-slashed-atpt ()
  "Hides slashed at point. "
  (interactive)
  (ar-th-hide 'slashed))

(defun ar-show-slashed-atpt ()
  "Shows hidden slashed at point. "
  (interactive)
  (ar-th-show 'slashed))

(defun ar-hide-show-slashed-atpt ()
  "Alternatively hides or shows slashed at point. "
  (interactive)
  (ar-th-hide-show 'slashed))

(defun ar-highlight-slashed-atpt-mode (&optional no-delimiters)
  "Toggles slashed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'slashed no-delimiters (interactive-p)))

(defun ar-kill-slashed-atpt (&optional no-delimiters)
  "Kills slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'slashed no-delimiters (interactive-p)))

(defun ar-kill-backward-slashed-atpt (&optional no-delimiters)
  "Kills slashed at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'slashed no-delimiters (interactive-p)))

(defun ar-separate-slashed-atpt (&optional no-delimiters)
  "Separates slashed at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'slashed no-delimiters (interactive-p)))

(defun ar-forward-slashed-atpt (&optional arg)
  "Moves forward over slashed at point if any, does nothing otherwise.
Returns end position of slashed "
  (interactive "p")
  (ar-th-forward 'slashed arg (interactive-p)))

(defun ar-backward-slashed-atpt (&optional arg)
  "Moves backward over slashed before point if any, does nothing otherwise.
Returns beginning position of slashed "
  (interactive "p")
  (ar-th-backward 'slashed arg (interactive-p)))

(defun ar-transpose-slashed-atpt (&optional arg)
  "Transposes slashed with slashed before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'slashed arg (interactive-p)))

(defun ar-sort-slashed-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts slasheds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'slashed reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-slashed-atpt ()
  "Return t if a slashed at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-slashed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-slashed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-stared-atpt (&optional arg no-delimiters)
  "Returns stared at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'stared arg no-delimiters (interactive-p)))

(defun ar-bounds-of-stared-atpt (&optional no-delimiters)
  "Returns a list, borders of stared if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'stared no-delimiters (interactive-p)))

(defun ar-stared-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position stared at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'stared no-delimiters (interactive-p)))

(defun ar-stared-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of stared at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'stared no-delimiters (interactive-p)))

(defun ar-beginning-of-stared-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class stared at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'stared no-delimiters (interactive-p)))

(defun ar-end-of-stared-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class stared at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'stared no-delimiters (interactive-p)))

(defun ar-length-of-stared-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class stared at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'stared no-delimiters (interactive-p)))

(defun ar-copy-stared-atpt (&optional no-delimiters)
  "Returns a copy of stared at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'stared no-delimiters (interactive-p)))

(defun ar-delete-stared-atpt (&optional arg)
  "Deletes stared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'stared arg (interactive-p)))

(defun ar-delete-stared-in-region (beg end)
  "Deletes stared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'stared beg end (interactive-p)))

(defun ar-blok-stared-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around stared.

If region is active, do that for all elements \"stared\" in region.
  Returns blok or nil if no stared at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'stared no-delimiters (interactive-p)))

(defun ar-doublebackslash-stared-atpt (&optional no-delimiters)
  "Puts doubled backslashes around stared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'stared no-delimiters (interactive-p)))

(defun ar-doubleslash-stared-atpt (&optional no-delimiters)
  "Puts doubled slashes around stared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'stared no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-stared-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around stared at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'stared no-delimiters (interactive-p)))

;; (defun ar-slashparen-stared-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around stared at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'stared no-delimiters (interactive-p)))

(defun ar-comment-stared-atpt (&optional no-delimiters)
  "Comments stared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'stared no-delimiters (interactive-p)))

(defun ar-commatize-stared-atpt (&optional no-delimiters)
  "Put a comma after stared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'stared no-delimiters (interactive-p)))

(defun ar-mark-stared-atpt (&optional no-delimiters)
  "Marks stared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'stared))

(defun ar-hide-stared-atpt ()
  "Hides stared at point. "
  (interactive)
  (ar-th-hide 'stared))

(defun ar-show-stared-atpt ()
  "Shows hidden stared at point. "
  (interactive)
  (ar-th-show 'stared))

(defun ar-hide-show-stared-atpt ()
  "Alternatively hides or shows stared at point. "
  (interactive)
  (ar-th-hide-show 'stared))

(defun ar-highlight-stared-atpt-mode (&optional no-delimiters)
  "Toggles stared-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'stared no-delimiters (interactive-p)))

(defun ar-kill-stared-atpt (&optional no-delimiters)
  "Kills stared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'stared no-delimiters (interactive-p)))

(defun ar-kill-backward-stared-atpt (&optional no-delimiters)
  "Kills stared at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'stared no-delimiters (interactive-p)))

(defun ar-separate-stared-atpt (&optional no-delimiters)
  "Separates stared at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'stared no-delimiters (interactive-p)))

(defun ar-forward-stared-atpt (&optional arg)
  "Moves forward over stared at point if any, does nothing otherwise.
Returns end position of stared "
  (interactive "p")
  (ar-th-forward 'stared arg (interactive-p)))

(defun ar-backward-stared-atpt (&optional arg)
  "Moves backward over stared before point if any, does nothing otherwise.
Returns beginning position of stared "
  (interactive "p")
  (ar-th-backward 'stared arg (interactive-p)))

(defun ar-transpose-stared-atpt (&optional arg)
  "Transposes stared with stared before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'stared arg (interactive-p)))

(defun ar-sort-stared-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts stareds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'stared reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-stared-atpt ()
  "Return t if a stared at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-stared-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-stared-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-tilded-atpt (&optional arg no-delimiters)
  "Returns tilded at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'tilded arg no-delimiters (interactive-p)))

(defun ar-bounds-of-tilded-atpt (&optional no-delimiters)
  "Returns a list, borders of tilded if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'tilded no-delimiters (interactive-p)))

(defun ar-tilded-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position tilded at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'tilded no-delimiters (interactive-p)))

(defun ar-tilded-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of tilded at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'tilded no-delimiters (interactive-p)))

(defun ar-beginning-of-tilded-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class tilded at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'tilded no-delimiters (interactive-p)))

(defun ar-end-of-tilded-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class tilded at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'tilded no-delimiters (interactive-p)))

(defun ar-length-of-tilded-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class tilded at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'tilded no-delimiters (interactive-p)))

(defun ar-copy-tilded-atpt (&optional no-delimiters)
  "Returns a copy of tilded at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'tilded no-delimiters (interactive-p)))

(defun ar-delete-tilded-atpt (&optional arg)
  "Deletes tilded at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'tilded arg (interactive-p)))

(defun ar-delete-tilded-in-region (beg end)
  "Deletes tilded at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'tilded beg end (interactive-p)))

(defun ar-blok-tilded-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around tilded.

If region is active, do that for all elements \"tilded\" in region.
  Returns blok or nil if no tilded at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'tilded no-delimiters (interactive-p)))

(defun ar-doublebackslash-tilded-atpt (&optional no-delimiters)
  "Puts doubled backslashes around tilded at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'tilded no-delimiters (interactive-p)))

(defun ar-doubleslash-tilded-atpt (&optional no-delimiters)
  "Puts doubled slashes around tilded at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'tilded no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-tilded-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around tilded at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'tilded no-delimiters (interactive-p)))

;; (defun ar-slashparen-tilded-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around tilded at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'tilded no-delimiters (interactive-p)))

(defun ar-comment-tilded-atpt (&optional no-delimiters)
  "Comments tilded at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'tilded no-delimiters (interactive-p)))

(defun ar-commatize-tilded-atpt (&optional no-delimiters)
  "Put a comma after tilded at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'tilded no-delimiters (interactive-p)))

(defun ar-mark-tilded-atpt (&optional no-delimiters)
  "Marks tilded at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'tilded))

(defun ar-hide-tilded-atpt ()
  "Hides tilded at point. "
  (interactive)
  (ar-th-hide 'tilded))

(defun ar-show-tilded-atpt ()
  "Shows hidden tilded at point. "
  (interactive)
  (ar-th-show 'tilded))

(defun ar-hide-show-tilded-atpt ()
  "Alternatively hides or shows tilded at point. "
  (interactive)
  (ar-th-hide-show 'tilded))

(defun ar-highlight-tilded-atpt-mode (&optional no-delimiters)
  "Toggles tilded-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'tilded no-delimiters (interactive-p)))

(defun ar-kill-tilded-atpt (&optional no-delimiters)
  "Kills tilded at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'tilded no-delimiters (interactive-p)))

(defun ar-kill-backward-tilded-atpt (&optional no-delimiters)
  "Kills tilded at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'tilded no-delimiters (interactive-p)))

(defun ar-separate-tilded-atpt (&optional no-delimiters)
  "Separates tilded at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'tilded no-delimiters (interactive-p)))

(defun ar-forward-tilded-atpt (&optional arg)
  "Moves forward over tilded at point if any, does nothing otherwise.
Returns end position of tilded "
  (interactive "p")
  (ar-th-forward 'tilded arg (interactive-p)))

(defun ar-backward-tilded-atpt (&optional arg)
  "Moves backward over tilded before point if any, does nothing otherwise.
Returns beginning position of tilded "
  (interactive "p")
  (ar-th-backward 'tilded arg (interactive-p)))

(defun ar-transpose-tilded-atpt (&optional arg)
  "Transposes tilded with tilded before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'tilded arg (interactive-p)))

(defun ar-sort-tilded-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts tildeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'tilded reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-tilded-atpt ()
  "Return t if a tilded at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-tilded-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-tilded-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-underscored-atpt (&optional arg no-delimiters)
  "Returns underscored at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'underscored arg no-delimiters (interactive-p)))

(defun ar-bounds-of-underscored-atpt (&optional no-delimiters)
  "Returns a list, borders of underscored if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'underscored no-delimiters (interactive-p)))

(defun ar-underscored-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position underscored at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'underscored no-delimiters (interactive-p)))

(defun ar-underscored-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of underscored at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'underscored no-delimiters (interactive-p)))

(defun ar-beginning-of-underscored-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class underscored at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'underscored no-delimiters (interactive-p)))

(defun ar-end-of-underscored-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class underscored at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'underscored no-delimiters (interactive-p)))

(defun ar-length-of-underscored-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class underscored at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'underscored no-delimiters (interactive-p)))

(defun ar-copy-underscored-atpt (&optional no-delimiters)
  "Returns a copy of underscored at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'underscored no-delimiters (interactive-p)))

(defun ar-delete-underscored-atpt (&optional arg)
  "Deletes underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'underscored arg (interactive-p)))

(defun ar-delete-underscored-in-region (beg end)
  "Deletes underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'underscored beg end (interactive-p)))

(defun ar-blok-underscored-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around underscored.

If region is active, do that for all elements \"underscored\" in region.
  Returns blok or nil if no underscored at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'underscored no-delimiters (interactive-p)))

(defun ar-doublebackslash-underscored-atpt (&optional no-delimiters)
  "Puts doubled backslashes around underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'underscored no-delimiters (interactive-p)))

(defun ar-doubleslash-underscored-atpt (&optional no-delimiters)
  "Puts doubled slashes around underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'underscored no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-underscored-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around underscored at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'underscored no-delimiters (interactive-p)))

;; (defun ar-slashparen-underscored-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around underscored at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'underscored no-delimiters (interactive-p)))

(defun ar-comment-underscored-atpt (&optional no-delimiters)
  "Comments underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'underscored no-delimiters (interactive-p)))

(defun ar-commatize-underscored-atpt (&optional no-delimiters)
  "Put a comma after underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'underscored no-delimiters (interactive-p)))

(defun ar-mark-underscored-atpt (&optional no-delimiters)
  "Marks underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'underscored))

(defun ar-hide-underscored-atpt ()
  "Hides underscored at point. "
  (interactive)
  (ar-th-hide 'underscored))

(defun ar-show-underscored-atpt ()
  "Shows hidden underscored at point. "
  (interactive)
  (ar-th-show 'underscored))

(defun ar-hide-show-underscored-atpt ()
  "Alternatively hides or shows underscored at point. "
  (interactive)
  (ar-th-hide-show 'underscored))

(defun ar-highlight-underscored-atpt-mode (&optional no-delimiters)
  "Toggles underscored-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'underscored no-delimiters (interactive-p)))

(defun ar-kill-underscored-atpt (&optional no-delimiters)
  "Kills underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'underscored no-delimiters (interactive-p)))

(defun ar-kill-backward-underscored-atpt (&optional no-delimiters)
  "Kills underscored at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'underscored no-delimiters (interactive-p)))

(defun ar-separate-underscored-atpt (&optional no-delimiters)
  "Separates underscored at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'underscored no-delimiters (interactive-p)))

(defun ar-forward-underscored-atpt (&optional arg)
  "Moves forward over underscored at point if any, does nothing otherwise.
Returns end position of underscored "
  (interactive "p")
  (ar-th-forward 'underscored arg (interactive-p)))

(defun ar-backward-underscored-atpt (&optional arg)
  "Moves backward over underscored before point if any, does nothing otherwise.
Returns beginning position of underscored "
  (interactive "p")
  (ar-th-backward 'underscored arg (interactive-p)))

(defun ar-transpose-underscored-atpt (&optional arg)
  "Transposes underscored with underscored before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'underscored arg (interactive-p)))

(defun ar-sort-underscored-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts underscoreds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'underscored reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-underscored-atpt ()
  "Return t if a underscored at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-underscored-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-underscored-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-whitespaced-atpt (&optional arg no-delimiters)
  "Returns whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS, resp. to inner position of delimiting char or string "
  (interactive "p\nP")
  (ar-th 'whitespaced arg no-delimiters (interactive-p)))

(defun ar-bounds-of-whitespaced-atpt (&optional no-delimiters)
  "Returns a list, borders of whitespaced if any, nil otherwise.
With optional NO-DELIMITERS, return inner position of delimiting char or string. " (interactive "P")
  (ar-th-bounds 'whitespaced no-delimiters (interactive-p)))

(defun ar-whitespaced-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-beg 'whitespaced no-delimiters (interactive-p)))

(defun ar-whitespaced-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-end 'whitespaced no-delimiters (interactive-p)))

(defun ar-beginning-of-whitespaced-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotobeg 'whitespaced no-delimiters (interactive-p)))

(defun ar-end-of-whitespaced-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS, to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-gotoend 'whitespaced no-delimiters (interactive-p)))

(defun ar-length-of-whitespaced-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS, return inner position of delimiting char or string. "
  (interactive "P")
  (ar-th-length 'whitespaced no-delimiters (interactive-p)))

(defun ar-copy-whitespaced-atpt (&optional no-delimiters)
  "Returns a copy of whitespaced at point if any, nil otherwise.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-copy 'whitespaced no-delimiters (interactive-p)))

(defun ar-delete-whitespaced-atpt (&optional arg)
  "Deletes whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete 'whitespaced arg (interactive-p)))

(defun ar-delete-whitespaced-in-region (beg end)
  "Deletes whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*r")
  (ar-th-delete-in-region 'whitespaced beg end (interactive-p)))

(defun ar-blok-whitespaced-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around whitespaced.

If region is active, do that for all elements \"whitespaced\" in region.
  Returns blok or nil if no whitespaced at cursor-position.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-blok 'whitespaced no-delimiters (interactive-p)))

(defun ar-doublebackslash-whitespaced-atpt (&optional no-delimiters)
  "Puts doubled backslashes around whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslash 'whitespaced no-delimiters (interactive-p)))

(defun ar-doubleslash-whitespaced-atpt (&optional no-delimiters)
  "Puts doubled slashes around whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doubleslash 'whitespaced no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-whitespaced-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around whitespaced at point if any.
With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-doublebackslashparen 'whitespaced no-delimiters (interactive-p)))

;; (defun ar-slashparen-whitespaced-atpt (&optional no-delimiters)
;;   "Provides slashed parentheses around whitespaced at point if any.

;; With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
;;   (interactive "*p")
;;   (ar-th-slashparen 'whitespaced no-delimiters (interactive-p)))

(defun ar-comment-whitespaced-atpt (&optional no-delimiters)
  "Comments whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-comment 'whitespaced no-delimiters (interactive-p)))

(defun ar-commatize-whitespaced-atpt (&optional no-delimiters)
  "Put a comma after whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-commatize 'whitespaced no-delimiters (interactive-p)))

(defun ar-mark-whitespaced-atpt (&optional no-delimiters)
  "Marks whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "P")
  (ar-th-mark 'whitespaced))

(defun ar-hide-whitespaced-atpt ()
  "Hides whitespaced at point. "
  (interactive)
  (ar-th-hide 'whitespaced))

(defun ar-show-whitespaced-atpt ()
  "Shows hidden whitespaced at point. "
  (interactive)
  (ar-th-show 'whitespaced))

(defun ar-hide-show-whitespaced-atpt ()
  "Alternatively hides or shows whitespaced at point. "
  (interactive)
  (ar-th-hide-show 'whitespaced))

(defun ar-highlight-whitespaced-atpt-mode (&optional no-delimiters)
  "Toggles whitespaced-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'whitespaced no-delimiters (interactive-p)))

(defun ar-kill-whitespaced-atpt (&optional no-delimiters)
  "Kills whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill 'whitespaced no-delimiters (interactive-p)))

(defun ar-kill-backward-whitespaced-atpt (&optional no-delimiters)
  "Kills whitespaced at point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*P")
  (ar-th-kill-backward 'whitespaced no-delimiters (interactive-p)))

(defun ar-separate-whitespaced-atpt (&optional no-delimiters)
  "Separates whitespaced at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'whitespaced no-delimiters (interactive-p)))

(defun ar-forward-whitespaced-atpt (&optional arg)
  "Moves forward over whitespaced at point if any, does nothing otherwise.
Returns end position of whitespaced "
  (interactive "p")
  (ar-th-forward 'whitespaced arg (interactive-p)))

(defun ar-backward-whitespaced-atpt (&optional arg)
  "Moves backward over whitespaced before point if any, does nothing otherwise.
Returns beginning position of whitespaced "
  (interactive "p")
  (ar-th-backward 'whitespaced arg (interactive-p)))

(defun ar-transpose-whitespaced-atpt (&optional arg)
  "Transposes whitespaced with whitespaced before point if any.

With optional NO-DELIMITERS resp. to inner position of delimiting char or string "
  (interactive "*p")
  (ar-th-transpose 'whitespaced arg (interactive-p)))

(defun ar-sort-whitespaced-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts whitespaceds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'whitespaced reverse beg end startkeyfun endkeyfun predicate)))

(defun ar-check-whitespaced-atpt ()
  "Return t if a whitespaced at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-whitespaced-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-whitespaced-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimited-intern: ar-unpaired-delimited-passiv-raw end

;; ar-thing-at-point-utils-delimited-unpaired-anlegen: ar-unpaired-delimited-passiv-raw start

(defalias 'ar-in-backslashed-p-atpt 'ar-backslashed-in-p-atpt)
(defun ar-backslashed-in-p-atpt (&optional condition)
  "Returns beginning position of ` backslashed' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "\\\\" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-backticked-p-atpt 'ar-backticked-in-p-atpt)
(defun ar-backticked-in-p-atpt (&optional condition)
  "Returns beginning position of ` backticked' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "`" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-coloned-p-atpt 'ar-coloned-in-p-atpt)
(defun ar-coloned-in-p-atpt (&optional condition)
  "Returns beginning position of ` coloned' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base ":" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-crossed-p-atpt 'ar-crossed-in-p-atpt)
(defun ar-crossed-in-p-atpt (&optional condition)
  "Returns beginning position of ` crossed' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "+" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-dollared-p-atpt 'ar-dollared-in-p-atpt)
(defun ar-dollared-in-p-atpt (&optional condition)
  "Returns beginning position of ` dollared' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "\\$" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-doublequoted-p-atpt 'ar-doublequoted-in-p-atpt)
(defun ar-doublequoted-in-p-atpt (&optional condition)
  "Returns beginning position of ` doublequoted' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "\"" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-equalized-p-atpt 'ar-equalized-in-p-atpt)
(defun ar-equalized-in-p-atpt (&optional condition)
  "Returns beginning position of ` equalized' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "=" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-hashed-p-atpt 'ar-hashed-in-p-atpt)
(defun ar-hashed-in-p-atpt (&optional condition)
  "Returns beginning position of ` hashed' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "#" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-hyphened-p-atpt 'ar-hyphened-in-p-atpt)
(defun ar-hyphened-in-p-atpt (&optional condition)
  "Returns beginning position of ` hyphened' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "-" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-singlequoted-p-atpt 'ar-singlequoted-in-p-atpt)
(defun ar-singlequoted-in-p-atpt (&optional condition)
  "Returns beginning position of ` singlequoted' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "'" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-slashed-p-atpt 'ar-slashed-in-p-atpt)
(defun ar-slashed-in-p-atpt (&optional condition)
  "Returns beginning position of ` slashed' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "/" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-stared-p-atpt 'ar-stared-in-p-atpt)
(defun ar-stared-in-p-atpt (&optional condition)
  "Returns beginning position of ` stared' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "*" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-tilded-p-atpt 'ar-tilded-in-p-atpt)
(defun ar-tilded-in-p-atpt (&optional condition)
  "Returns beginning position of ` tilded' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "~" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-underscored-p-atpt 'ar-underscored-in-p-atpt)
(defun ar-underscored-in-p-atpt (&optional condition)
  "Returns beginning position of ` underscored' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base "_" condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

(defalias 'ar-in-whitespaced-p-atpt 'ar-whitespaced-in-p-atpt)
(defun ar-whitespaced-in-p-atpt (&optional condition)
  "Returns beginning position of ` whitespaced' if inside, a number or a list, nil otherwise.

Optional arg CONDITION accepts a function. If it returns `t', result at point is discarded, search continues.
Like check for in-comment, which is done internally."
  (interactive)
  (let ((erg (ar-in-delimiter-base " " condition)))
    (when (interactive-p) (message "%s" erg))
    erg))

;; ar-thing-at-point-utils-delimited-unpaired-anlegen: ar-unpaired-delimited-passiv-raw end

;; ar-thing-at-point-utils-delimiters-core: ar-paired-delimited-passiv start

(defun ar-braced-atpt (&optional arg no-delimiters)
  "Returns braced at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'braced arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-braced-atpt 'ar-braced-bounds-atpt)
(defun ar-braced-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of braced if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'braced no-delimiters (interactive-p)))

(defun ar-braced-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BRACED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'braced no-delimiters (interactive-p)))

(defun ar-braced-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'braced no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-braced-atpt 'ar-braced-beginning-atpt)
(defun ar-braced-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'braced no-delimiters (interactive-p)))

(defalias 'ar-end-of-braced-atpt 'ar-braced-end-atpt)
(defun ar-braced-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'braced no-delimiters (interactive-p)))

(defalias 'ar-in-braced-p-atpt 'ar-braced-in-p-atpt)
(defun ar-braced-in-p-atpt (&optional no-delimiters)
  "Returns bounds of BRACED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'braced no-delimiters (interactive-p)))

(defalias 'ar-length-of-braced-atpt 'ar-braced-length-atpt)
(defun ar-braced-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'braced no-delimiters (interactive-p)))

(defalias 'ar-copy-braced-atpt 'ar-braced-copy-atpt)
(defun ar-braced-copy-atpt (&optional no-delimiters)
  "Returns a copy of BRACED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'braced no-delimiters (interactive-p)))

(defalias 'ar-delete-braced-atpt 'ar-braced-delete-atpt)
(defun ar-braced-delete-atpt (&optional arg)
  "Deletes BRACED at point if any. "
  (interactive "*p")
  (ar-th-delete 'braced arg (interactive-p)))

(defalias 'ar-delete-braced-in-region 'ar-braced-delete-in-region)
(defun ar-braced-delete-in-region (beg end)
  "Deletes BRACED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'braced beg end (interactive-p)))

(defun ar-blok-braced-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around braced.
  Returns blok or nil if no BRACED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'braced no-delimiters (interactive-p)))

(defun ar-doublebackslash-braced-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BRACED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'braced no-delimiters (interactive-p)))

(defun ar-doubleslash-braced-atpt (&optional no-delimiters)
  "Puts doubled slashes around BRACED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'braced no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-braced-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BRACED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'braced no-delimiters (interactive-p)))

(defun ar-comment-braced-atpt (&optional no-delimiters)
  "Comments BRACED at point if any. "
  (interactive "*p")
  (ar-th-comment 'braced no-delimiters (interactive-p)))

(defun ar-commatize-braced-atpt (&optional no-delimiters)
  "Put a comma after BRACED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'braced no-delimiters (interactive-p)))

(defun ar-quote-braced-atpt (&optional no-delimiters)
  "Put a singlequote before BRACED at point if any. "
  (interactive "*p")
  (ar-th-quote 'braced no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-braced-atpt 'ar-braced-hyphen-atpt)
;; ;; (defun ar-braced-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around BRACED at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'braced no-delimiters (interactive-p)))

(defalias 'ar-mark-braced-atpt 'ar-braced-mark-atpt)
(defun ar-braced-mark-atpt ()
  "Marks BRACED at point if any. "
  (interactive)
  (ar-th-mark 'braced))

(defalias 'ar-hide-braced-atpt 'ar-braced-hide-atpt)
(defun ar-braced-hide-atpt ()
  "Hides BRACED at point. "
  (interactive)
  (ar-th-hide 'braced))

(defalias 'ar-show-braced-atpt 'ar-braced-show-atpt)
(defun ar-braced-show-atpt ()
  "Shows hidden BRACED at point. "
  (interactive)
  (ar-th-show 'braced))

(defalias 'ar-hide-show-braced-atpt 'ar-braced-hide-show-atpt)
(defun ar-braced-hide-show-atpt ()
  "Alternatively hides or shows BRACED at point. "
  (interactive)
  (ar-th-hide-show 'braced))

(defalias 'ar-highlight-braced-atpt-mode 'ar-braced-highlight-atpt-mode)

(defun ar-braced-highlight-atpt-mode (&optional no-delimiters)
  "Toggles braced-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'braced no-delimiters (interactive-p)))

(defalias 'ar-kill-braced-atpt 'ar-braced-kill-atpt)
(defun ar-braced-kill-atpt (&optional no-delimiters)
  "Kills BRACED at point if any. "
  (interactive "*P")
  (ar-th-kill 'braced no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-braced-atpt 'ar-braced-kill-backward-atpt)
(defun ar-braced-kill-backward-atpt (&optional no-delimiters)
  "Kills BRACED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'braced no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-braced-atpt 'ar-braced-leftrightsinglequote-atpt)
;; ;; (defun ar-braced-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'braced no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-braced-atpt 'ar-braced-parentize-atpt)
;; ;; (defun ar-braced-parentize-atpt (&optional no-delimiters)
;;   "Parentizes BRACED at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'braced no-delimiters (interactive-p)))

(defalias 'ar-separate-braced-atpt 'ar-braced-separate-atpt)
(defun ar-braced-separate-atpt (&optional no-delimiters)
  "Separates BRACED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'braced no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-braced-atpt 'ar-braced-singlequote-atpt)
;; ;; (defun ar-braced-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes BRACED at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'braced no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-braced-atpt 'ar-braced-triplequotedq-atpt)
(defun ar-braced-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around braced. "
  (interactive "*p")
  (ar-th-triplequotedq 'braced no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-braced-atpt 'ar-braced-triplequotesq-atpt)
(defun ar-braced-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around braced. "
  (interactive "*p")
  (ar-th-triplequotesq 'braced no-delimiters (interactive-p)))

(defun ar-underscore-braced-atpt (&optional no-delimiters)
  "Put underscore char around BRACED. "
  (interactive "*p")
  (ar-th-underscore 'braced no-delimiters (interactive-p)))

;; (defalias 'ar-braced-whitespace-atpt 'ar-whitespace-braced-atpt)
;; ;; (defun ar-whitespace-braced-atpt (&optional no-delimiters)
;;   "Put whitespace char around BRACED. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'braced nil t))

(defalias 'ar-forward-braced-atpt 'ar-braced-forward-atpt)
(defun ar-braced-forward-atpt (&optional arg)
  "Moves forward over BRACED at point if any, does nothing otherwise.
Returns end position of BRACED "
  (interactive "p")
  (ar-th-forward 'braced arg (interactive-p)))

(defalias 'ar-backward-braced-atpt 'ar-braced-backward-atpt)
(defun ar-braced-backward-atpt (&optional arg)
  "Moves backward over BRACED before point if any, does nothing otherwise.
Returns beginning position of BRACED "
  (interactive "p")
  (ar-th-backward 'braced arg (interactive-p)))

(defalias 'ar-transpose-braced-atpt 'ar-braced-transpose-atpt)
(defun ar-braced-transpose-atpt (&optional arg)
  "Transposes BRACED with BRACED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'braced arg (interactive-p)))

(defalias 'ar-sort-braced-atpt 'ar-braced-sort-atpt)
(defun ar-braced-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts braceds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'braced reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-braced-atpt 'ar-braced-check-atpt)
(defun ar-braced-check-atpt ()
  "Return t if a BRACED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-braced-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-braced-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-bracketed-atpt (&optional arg no-delimiters)
  "Returns bracketed at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'bracketed arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-bracketed-atpt 'ar-bracketed-bounds-atpt)
(defun ar-bracketed-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of bracketed if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'bracketed no-delimiters (interactive-p)))

(defun ar-bracketed-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BRACKETED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'bracketed no-delimiters (interactive-p)))

(defun ar-bracketed-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-bracketed-atpt 'ar-bracketed-beginning-atpt)
(defun ar-bracketed-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-end-of-bracketed-atpt 'ar-bracketed-end-atpt)
(defun ar-bracketed-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-in-bracketed-p-atpt 'ar-bracketed-in-p-atpt)
(defun ar-bracketed-in-p-atpt (&optional no-delimiters)
  "Returns bounds of BRACKETED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-length-of-bracketed-atpt 'ar-bracketed-length-atpt)
(defun ar-bracketed-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-copy-bracketed-atpt 'ar-bracketed-copy-atpt)
(defun ar-bracketed-copy-atpt (&optional no-delimiters)
  "Returns a copy of BRACKETED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-delete-bracketed-atpt 'ar-bracketed-delete-atpt)
(defun ar-bracketed-delete-atpt (&optional arg)
  "Deletes BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-delete 'bracketed arg (interactive-p)))

(defalias 'ar-delete-bracketed-in-region 'ar-bracketed-delete-in-region)
(defun ar-bracketed-delete-in-region (beg end)
  "Deletes BRACKETED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'bracketed beg end (interactive-p)))

(defun ar-blok-bracketed-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around bracketed.
  Returns blok or nil if no BRACKETED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'bracketed no-delimiters (interactive-p)))

(defun ar-doublebackslash-bracketed-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'bracketed no-delimiters (interactive-p)))

(defun ar-doubleslash-bracketed-atpt (&optional no-delimiters)
  "Puts doubled slashes around BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'bracketed no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-bracketed-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'bracketed no-delimiters (interactive-p)))

(defun ar-comment-bracketed-atpt (&optional no-delimiters)
  "Comments BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-comment 'bracketed no-delimiters (interactive-p)))

(defun ar-commatize-bracketed-atpt (&optional no-delimiters)
  "Put a comma after BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'bracketed no-delimiters (interactive-p)))

(defun ar-quote-bracketed-atpt (&optional no-delimiters)
  "Put a singlequote before BRACKETED at point if any. "
  (interactive "*p")
  (ar-th-quote 'bracketed no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-bracketed-atpt 'ar-bracketed-hyphen-atpt)
;; ;; (defun ar-bracketed-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around BRACKETED at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-mark-bracketed-atpt 'ar-bracketed-mark-atpt)
(defun ar-bracketed-mark-atpt ()
  "Marks BRACKETED at point if any. "
  (interactive)
  (ar-th-mark 'bracketed))

(defalias 'ar-hide-bracketed-atpt 'ar-bracketed-hide-atpt)
(defun ar-bracketed-hide-atpt ()
  "Hides BRACKETED at point. "
  (interactive)
  (ar-th-hide 'bracketed))

(defalias 'ar-show-bracketed-atpt 'ar-bracketed-show-atpt)
(defun ar-bracketed-show-atpt ()
  "Shows hidden BRACKETED at point. "
  (interactive)
  (ar-th-show 'bracketed))

(defalias 'ar-hide-show-bracketed-atpt 'ar-bracketed-hide-show-atpt)
(defun ar-bracketed-hide-show-atpt ()
  "Alternatively hides or shows BRACKETED at point. "
  (interactive)
  (ar-th-hide-show 'bracketed))

(defalias 'ar-highlight-bracketed-atpt-mode 'ar-bracketed-highlight-atpt-mode)

(defun ar-bracketed-highlight-atpt-mode (&optional no-delimiters)
  "Toggles bracketed-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-kill-bracketed-atpt 'ar-bracketed-kill-atpt)
(defun ar-bracketed-kill-atpt (&optional no-delimiters)
  "Kills BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-kill 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-bracketed-atpt 'ar-bracketed-kill-backward-atpt)
(defun ar-bracketed-kill-backward-atpt (&optional no-delimiters)
  "Kills BRACKETED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'bracketed no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-bracketed-atpt 'ar-bracketed-leftrightsinglequote-atpt)
;; ;; (defun ar-bracketed-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'bracketed no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-bracketed-atpt 'ar-bracketed-parentize-atpt)
;; ;; (defun ar-bracketed-parentize-atpt (&optional no-delimiters)
;;   "Parentizes BRACKETED at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-separate-bracketed-atpt 'ar-bracketed-separate-atpt)
(defun ar-bracketed-separate-atpt (&optional no-delimiters)
  "Separates BRACKETED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'bracketed no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-bracketed-atpt 'ar-bracketed-singlequote-atpt)
;; ;; (defun ar-bracketed-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes BRACKETED at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-bracketed-atpt 'ar-bracketed-triplequotedq-atpt)
(defun ar-bracketed-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around bracketed. "
  (interactive "*p")
  (ar-th-triplequotedq 'bracketed no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-bracketed-atpt 'ar-bracketed-triplequotesq-atpt)
(defun ar-bracketed-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around bracketed. "
  (interactive "*p")
  (ar-th-triplequotesq 'bracketed no-delimiters (interactive-p)))

(defun ar-underscore-bracketed-atpt (&optional no-delimiters)
  "Put underscore char around BRACKETED. "
  (interactive "*p")
  (ar-th-underscore 'bracketed no-delimiters (interactive-p)))

;; (defalias 'ar-bracketed-whitespace-atpt 'ar-whitespace-bracketed-atpt)
;; ;; (defun ar-whitespace-bracketed-atpt (&optional no-delimiters)
;;   "Put whitespace char around BRACKETED. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'bracketed nil t))

(defalias 'ar-forward-bracketed-atpt 'ar-bracketed-forward-atpt)
(defun ar-bracketed-forward-atpt (&optional arg)
  "Moves forward over BRACKETED at point if any, does nothing otherwise.
Returns end position of BRACKETED "
  (interactive "p")
  (ar-th-forward 'bracketed arg (interactive-p)))

(defalias 'ar-backward-bracketed-atpt 'ar-bracketed-backward-atpt)
(defun ar-bracketed-backward-atpt (&optional arg)
  "Moves backward over BRACKETED before point if any, does nothing otherwise.
Returns beginning position of BRACKETED "
  (interactive "p")
  (ar-th-backward 'bracketed arg (interactive-p)))

(defalias 'ar-transpose-bracketed-atpt 'ar-bracketed-transpose-atpt)
(defun ar-bracketed-transpose-atpt (&optional arg)
  "Transposes BRACKETED with BRACKETED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'bracketed arg (interactive-p)))

(defalias 'ar-sort-bracketed-atpt 'ar-bracketed-sort-atpt)
(defun ar-bracketed-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts bracketeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'bracketed reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-bracketed-atpt 'ar-bracketed-check-atpt)
(defun ar-bracketed-check-atpt ()
  "Return t if a BRACKETED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-bracketed-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-bracketed-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-lesserangled-atpt (&optional arg no-delimiters)
  "Returns lesserangled at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'lesserangled arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-lesserangled-atpt 'ar-lesserangled-bounds-atpt)
(defun ar-lesserangled-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of lesserangled if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lesserangled no-delimiters (interactive-p)))

(defun ar-lesserangled-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position LESSERANGLED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'lesserangled no-delimiters (interactive-p)))

(defun ar-lesserangled-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of LESSERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'lesserangled no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-lesserangled-atpt 'ar-lesserangled-beginning-atpt)
(defun ar-lesserangled-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class LESSERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'lesserangled no-delimiters (interactive-p)))

(defalias 'ar-end-of-lesserangled-atpt 'ar-lesserangled-end-atpt)
(defun ar-lesserangled-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class LESSERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'lesserangled no-delimiters (interactive-p)))

(defalias 'ar-in-lesserangled-p-atpt 'ar-lesserangled-in-p-atpt)
(defun ar-lesserangled-in-p-atpt (&optional no-delimiters)
  "Returns bounds of LESSERANGLED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lesserangled no-delimiters (interactive-p)))

(defalias 'ar-length-of-lesserangled-atpt 'ar-lesserangled-length-atpt)
(defun ar-lesserangled-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class LESSERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'lesserangled no-delimiters (interactive-p)))

(defalias 'ar-copy-lesserangled-atpt 'ar-lesserangled-copy-atpt)
(defun ar-lesserangled-copy-atpt (&optional no-delimiters)
  "Returns a copy of LESSERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'lesserangled no-delimiters (interactive-p)))

(defalias 'ar-delete-lesserangled-atpt 'ar-lesserangled-delete-atpt)
(defun ar-lesserangled-delete-atpt (&optional arg)
  "Deletes LESSERANGLED at point if any. "
  (interactive "*p")
  (ar-th-delete 'lesserangled arg (interactive-p)))

(defalias 'ar-delete-lesserangled-in-region 'ar-lesserangled-delete-in-region)
(defun ar-lesserangled-delete-in-region (beg end)
  "Deletes LESSERANGLED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'lesserangled beg end (interactive-p)))

(defun ar-blok-lesserangled-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around lesserangled.
  Returns blok or nil if no LESSERANGLED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'lesserangled no-delimiters (interactive-p)))

(defun ar-doublebackslash-lesserangled-atpt (&optional no-delimiters)
  "Puts doubled backslashes around LESSERANGLED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'lesserangled no-delimiters (interactive-p)))

(defun ar-doubleslash-lesserangled-atpt (&optional no-delimiters)
  "Puts doubled slashes around LESSERANGLED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'lesserangled no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-lesserangled-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around LESSERANGLED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'lesserangled no-delimiters (interactive-p)))

(defun ar-comment-lesserangled-atpt (&optional no-delimiters)
  "Comments LESSERANGLED at point if any. "
  (interactive "*p")
  (ar-th-comment 'lesserangled no-delimiters (interactive-p)))

(defun ar-commatize-lesserangled-atpt (&optional no-delimiters)
  "Put a comma after LESSERANGLED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'lesserangled no-delimiters (interactive-p)))

(defun ar-quote-lesserangled-atpt (&optional no-delimiters)
  "Put a singlequote before LESSERANGLED at point if any. "
  (interactive "*p")
  (ar-th-quote 'lesserangled no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-lesserangled-atpt 'ar-lesserangled-hyphen-atpt)
;; ;; (defun ar-lesserangled-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around LESSERANGLED at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'lesserangled no-delimiters (interactive-p)))

(defalias 'ar-mark-lesserangled-atpt 'ar-lesserangled-mark-atpt)
(defun ar-lesserangled-mark-atpt ()
  "Marks LESSERANGLED at point if any. "
  (interactive)
  (ar-th-mark 'lesserangled))

(defalias 'ar-hide-lesserangled-atpt 'ar-lesserangled-hide-atpt)
(defun ar-lesserangled-hide-atpt ()
  "Hides LESSERANGLED at point. "
  (interactive)
  (ar-th-hide 'lesserangled))

(defalias 'ar-show-lesserangled-atpt 'ar-lesserangled-show-atpt)
(defun ar-lesserangled-show-atpt ()
  "Shows hidden LESSERANGLED at point. "
  (interactive)
  (ar-th-show 'lesserangled))

(defalias 'ar-hide-show-lesserangled-atpt 'ar-lesserangled-hide-show-atpt)
(defun ar-lesserangled-hide-show-atpt ()
  "Alternatively hides or shows LESSERANGLED at point. "
  (interactive)
  (ar-th-hide-show 'lesserangled))

(defalias 'ar-highlight-lesserangled-atpt-mode 'ar-lesserangled-highlight-atpt-mode)

(defun ar-lesserangled-highlight-atpt-mode (&optional no-delimiters)
  "Toggles lesserangled-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'lesserangled no-delimiters (interactive-p)))

(defalias 'ar-kill-lesserangled-atpt 'ar-lesserangled-kill-atpt)
(defun ar-lesserangled-kill-atpt (&optional no-delimiters)
  "Kills LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-kill 'lesserangled no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-lesserangled-atpt 'ar-lesserangled-kill-backward-atpt)
(defun ar-lesserangled-kill-backward-atpt (&optional no-delimiters)
  "Kills LESSERANGLED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'lesserangled no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-lesserangled-atpt 'ar-lesserangled-leftrightsinglequote-atpt)
;; ;; (defun ar-lesserangled-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'lesserangled no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-lesserangled-atpt 'ar-lesserangled-parentize-atpt)
;; ;; (defun ar-lesserangled-parentize-atpt (&optional no-delimiters)
;;   "Parentizes LESSERANGLED at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'lesserangled no-delimiters (interactive-p)))

(defalias 'ar-separate-lesserangled-atpt 'ar-lesserangled-separate-atpt)
(defun ar-lesserangled-separate-atpt (&optional no-delimiters)
  "Separates LESSERANGLED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'lesserangled no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-lesserangled-atpt 'ar-lesserangled-singlequote-atpt)
;; ;; (defun ar-lesserangled-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes LESSERANGLED at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'lesserangled no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-lesserangled-atpt 'ar-lesserangled-triplequotedq-atpt)
(defun ar-lesserangled-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around lesserangled. "
  (interactive "*p")
  (ar-th-triplequotedq 'lesserangled no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-lesserangled-atpt 'ar-lesserangled-triplequotesq-atpt)
(defun ar-lesserangled-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around lesserangled. "
  (interactive "*p")
  (ar-th-triplequotesq 'lesserangled no-delimiters (interactive-p)))

(defun ar-underscore-lesserangled-atpt (&optional no-delimiters)
  "Put underscore char around LESSERANGLED. "
  (interactive "*p")
  (ar-th-underscore 'lesserangled no-delimiters (interactive-p)))

;; (defalias 'ar-lesserangled-whitespace-atpt 'ar-whitespace-lesserangled-atpt)
;; ;; (defun ar-whitespace-lesserangled-atpt (&optional no-delimiters)
;;   "Put whitespace char around LESSERANGLED. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'lesserangled nil t))

(defalias 'ar-forward-lesserangled-atpt 'ar-lesserangled-forward-atpt)
(defun ar-lesserangled-forward-atpt (&optional arg)
  "Moves forward over LESSERANGLED at point if any, does nothing otherwise.
Returns end position of LESSERANGLED "
  (interactive "p")
  (ar-th-forward 'lesserangled arg (interactive-p)))

(defalias 'ar-backward-lesserangled-atpt 'ar-lesserangled-backward-atpt)
(defun ar-lesserangled-backward-atpt (&optional arg)
  "Moves backward over LESSERANGLED before point if any, does nothing otherwise.
Returns beginning position of LESSERANGLED "
  (interactive "p")
  (ar-th-backward 'lesserangled arg (interactive-p)))

(defalias 'ar-transpose-lesserangled-atpt 'ar-lesserangled-transpose-atpt)
(defun ar-lesserangled-transpose-atpt (&optional arg)
  "Transposes LESSERANGLED with LESSERANGLED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'lesserangled arg (interactive-p)))

(defalias 'ar-sort-lesserangled-atpt 'ar-lesserangled-sort-atpt)
(defun ar-lesserangled-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lesserangleds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'lesserangled reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-lesserangled-atpt 'ar-lesserangled-check-atpt)
(defun ar-lesserangled-check-atpt ()
  "Return t if a LESSERANGLED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-lesserangled-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-lesserangled-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-greaterangled-atpt (&optional arg no-delimiters)
  "Returns greaterangled at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'greaterangled arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-greaterangled-atpt 'ar-greaterangled-bounds-atpt)
(defun ar-greaterangled-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of greaterangled if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'greaterangled no-delimiters (interactive-p)))

(defun ar-greaterangled-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position GREATERANGLED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'greaterangled no-delimiters (interactive-p)))

(defun ar-greaterangled-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of GREATERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'greaterangled no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-greaterangled-atpt 'ar-greaterangled-beginning-atpt)
(defun ar-greaterangled-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class GREATERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'greaterangled no-delimiters (interactive-p)))

(defalias 'ar-end-of-greaterangled-atpt 'ar-greaterangled-end-atpt)
(defun ar-greaterangled-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class GREATERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'greaterangled no-delimiters (interactive-p)))

(defalias 'ar-in-greaterangled-p-atpt 'ar-greaterangled-in-p-atpt)
(defun ar-greaterangled-in-p-atpt (&optional no-delimiters)
  "Returns bounds of GREATERANGLED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'greaterangled no-delimiters (interactive-p)))

(defalias 'ar-length-of-greaterangled-atpt 'ar-greaterangled-length-atpt)
(defun ar-greaterangled-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class GREATERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'greaterangled no-delimiters (interactive-p)))

(defalias 'ar-copy-greaterangled-atpt 'ar-greaterangled-copy-atpt)
(defun ar-greaterangled-copy-atpt (&optional no-delimiters)
  "Returns a copy of GREATERANGLED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'greaterangled no-delimiters (interactive-p)))

(defalias 'ar-delete-greaterangled-atpt 'ar-greaterangled-delete-atpt)
(defun ar-greaterangled-delete-atpt (&optional arg)
  "Deletes GREATERANGLED at point if any. "
  (interactive "*p")
  (ar-th-delete 'greaterangled arg (interactive-p)))

(defalias 'ar-delete-greaterangled-in-region 'ar-greaterangled-delete-in-region)
(defun ar-greaterangled-delete-in-region (beg end)
  "Deletes GREATERANGLED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'greaterangled beg end (interactive-p)))

(defun ar-blok-greaterangled-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around greaterangled.
  Returns blok or nil if no GREATERANGLED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'greaterangled no-delimiters (interactive-p)))

(defun ar-doublebackslash-greaterangled-atpt (&optional no-delimiters)
  "Puts doubled backslashes around GREATERANGLED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'greaterangled no-delimiters (interactive-p)))

(defun ar-doubleslash-greaterangled-atpt (&optional no-delimiters)
  "Puts doubled slashes around GREATERANGLED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'greaterangled no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-greaterangled-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around GREATERANGLED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'greaterangled no-delimiters (interactive-p)))

(defun ar-comment-greaterangled-atpt (&optional no-delimiters)
  "Comments GREATERANGLED at point if any. "
  (interactive "*p")
  (ar-th-comment 'greaterangled no-delimiters (interactive-p)))

(defun ar-commatize-greaterangled-atpt (&optional no-delimiters)
  "Put a comma after GREATERANGLED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'greaterangled no-delimiters (interactive-p)))

(defun ar-quote-greaterangled-atpt (&optional no-delimiters)
  "Put a singlequote before GREATERANGLED at point if any. "
  (interactive "*p")
  (ar-th-quote 'greaterangled no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-greaterangled-atpt 'ar-greaterangled-hyphen-atpt)
;; ;; (defun ar-greaterangled-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around GREATERANGLED at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'greaterangled no-delimiters (interactive-p)))

(defalias 'ar-mark-greaterangled-atpt 'ar-greaterangled-mark-atpt)
(defun ar-greaterangled-mark-atpt ()
  "Marks GREATERANGLED at point if any. "
  (interactive)
  (ar-th-mark 'greaterangled))

(defalias 'ar-hide-greaterangled-atpt 'ar-greaterangled-hide-atpt)
(defun ar-greaterangled-hide-atpt ()
  "Hides GREATERANGLED at point. "
  (interactive)
  (ar-th-hide 'greaterangled))

(defalias 'ar-show-greaterangled-atpt 'ar-greaterangled-show-atpt)
(defun ar-greaterangled-show-atpt ()
  "Shows hidden GREATERANGLED at point. "
  (interactive)
  (ar-th-show 'greaterangled))

(defalias 'ar-hide-show-greaterangled-atpt 'ar-greaterangled-hide-show-atpt)
(defun ar-greaterangled-hide-show-atpt ()
  "Alternatively hides or shows GREATERANGLED at point. "
  (interactive)
  (ar-th-hide-show 'greaterangled))

(defalias 'ar-highlight-greaterangled-atpt-mode 'ar-greaterangled-highlight-atpt-mode)

(defun ar-greaterangled-highlight-atpt-mode (&optional no-delimiters)
  "Toggles greaterangled-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'greaterangled no-delimiters (interactive-p)))

(defalias 'ar-kill-greaterangled-atpt 'ar-greaterangled-kill-atpt)
(defun ar-greaterangled-kill-atpt (&optional no-delimiters)
  "Kills GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-kill 'greaterangled no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-greaterangled-atpt 'ar-greaterangled-kill-backward-atpt)
(defun ar-greaterangled-kill-backward-atpt (&optional no-delimiters)
  "Kills GREATERANGLED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'greaterangled no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-greaterangled-atpt 'ar-greaterangled-leftrightsinglequote-atpt)
;; ;; (defun ar-greaterangled-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'greaterangled no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-greaterangled-atpt 'ar-greaterangled-parentize-atpt)
;; ;; (defun ar-greaterangled-parentize-atpt (&optional no-delimiters)
;;   "Parentizes GREATERANGLED at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'greaterangled no-delimiters (interactive-p)))

(defalias 'ar-separate-greaterangled-atpt 'ar-greaterangled-separate-atpt)
(defun ar-greaterangled-separate-atpt (&optional no-delimiters)
  "Separates GREATERANGLED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'greaterangled no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-greaterangled-atpt 'ar-greaterangled-singlequote-atpt)
;; ;; (defun ar-greaterangled-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes GREATERANGLED at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'greaterangled no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-greaterangled-atpt 'ar-greaterangled-triplequotedq-atpt)
(defun ar-greaterangled-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around greaterangled. "
  (interactive "*p")
  (ar-th-triplequotedq 'greaterangled no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-greaterangled-atpt 'ar-greaterangled-triplequotesq-atpt)
(defun ar-greaterangled-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around greaterangled. "
  (interactive "*p")
  (ar-th-triplequotesq 'greaterangled no-delimiters (interactive-p)))

(defun ar-underscore-greaterangled-atpt (&optional no-delimiters)
  "Put underscore char around GREATERANGLED. "
  (interactive "*p")
  (ar-th-underscore 'greaterangled no-delimiters (interactive-p)))

;; (defalias 'ar-greaterangled-whitespace-atpt 'ar-whitespace-greaterangled-atpt)
;; ;; (defun ar-whitespace-greaterangled-atpt (&optional no-delimiters)
;;   "Put whitespace char around GREATERANGLED. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'greaterangled nil t))

(defalias 'ar-forward-greaterangled-atpt 'ar-greaterangled-forward-atpt)
(defun ar-greaterangled-forward-atpt (&optional arg)
  "Moves forward over GREATERANGLED at point if any, does nothing otherwise.
Returns end position of GREATERANGLED "
  (interactive "p")
  (ar-th-forward 'greaterangled arg (interactive-p)))

(defalias 'ar-backward-greaterangled-atpt 'ar-greaterangled-backward-atpt)
(defun ar-greaterangled-backward-atpt (&optional arg)
  "Moves backward over GREATERANGLED before point if any, does nothing otherwise.
Returns beginning position of GREATERANGLED "
  (interactive "p")
  (ar-th-backward 'greaterangled arg (interactive-p)))

(defalias 'ar-transpose-greaterangled-atpt 'ar-greaterangled-transpose-atpt)
(defun ar-greaterangled-transpose-atpt (&optional arg)
  "Transposes GREATERANGLED with GREATERANGLED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'greaterangled arg (interactive-p)))

(defalias 'ar-sort-greaterangled-atpt 'ar-greaterangled-sort-atpt)
(defun ar-greaterangled-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts greaterangleds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'greaterangled reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-greaterangled-atpt 'ar-greaterangled-check-atpt)
(defun ar-greaterangled-check-atpt ()
  "Return t if a GREATERANGLED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-greaterangled-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-greaterangled-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-leftrightsinglequoted-atpt (&optional arg no-delimiters)
  "Returns leftrightsinglequoted at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'leftrightsinglequoted arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-bounds-atpt)
(defun ar-leftrightsinglequoted-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of leftrightsinglequoted if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-leftrightsinglequoted-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-leftrightsinglequoted-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'leftrightsinglequoted no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-beginning-atpt)
(defun ar-leftrightsinglequoted-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'leftrightsinglequoted no-delimiters (interactive-p)))

(defalias 'ar-end-of-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-end-atpt)
(defun ar-leftrightsinglequoted-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'leftrightsinglequoted no-delimiters (interactive-p)))

(defalias 'ar-in-leftrightsinglequoted-p-atpt 'ar-leftrightsinglequoted-in-p-atpt)
(defun ar-leftrightsinglequoted-in-p-atpt (&optional no-delimiters)
  "Returns bounds of LEFTRIGHTSINGLEQUOTED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'leftrightsinglequoted no-delimiters (interactive-p)))

(defalias 'ar-length-of-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-length-atpt)
(defun ar-leftrightsinglequoted-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'leftrightsinglequoted no-delimiters (interactive-p)))

(defalias 'ar-copy-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-copy-atpt)
(defun ar-leftrightsinglequoted-copy-atpt (&optional no-delimiters)
  "Returns a copy of LEFTRIGHTSINGLEQUOTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'leftrightsinglequoted no-delimiters (interactive-p)))

(defalias 'ar-delete-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-delete-atpt)
(defun ar-leftrightsinglequoted-delete-atpt (&optional arg)
  "Deletes LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-delete 'leftrightsinglequoted arg (interactive-p)))

(defalias 'ar-delete-leftrightsinglequoted-in-region 'ar-leftrightsinglequoted-delete-in-region)
(defun ar-leftrightsinglequoted-delete-in-region (beg end)
  "Deletes LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'leftrightsinglequoted beg end (interactive-p)))

(defun ar-blok-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around leftrightsinglequoted.
  Returns blok or nil if no LEFTRIGHTSINGLEQUOTED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-doublebackslash-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Puts doubled backslashes around LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-doubleslash-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Puts doubled slashes around LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-comment-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Comments LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-comment 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-commatize-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Put a comma after LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-quote-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Put a singlequote before LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*p")
  (ar-th-quote 'leftrightsinglequoted no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-hyphen-atpt)
;; ;; (defun ar-leftrightsinglequoted-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around LEFTRIGHTSINGLEQUOTED at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'leftrightsinglequoted no-delimiters (interactive-p)))

(defalias 'ar-mark-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-mark-atpt)
(defun ar-leftrightsinglequoted-mark-atpt ()
  "Marks LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive)
  (ar-th-mark 'leftrightsinglequoted))

(defalias 'ar-hide-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-hide-atpt)
(defun ar-leftrightsinglequoted-hide-atpt ()
  "Hides LEFTRIGHTSINGLEQUOTED at point. "
  (interactive)
  (ar-th-hide 'leftrightsinglequoted))

(defalias 'ar-show-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-show-atpt)
(defun ar-leftrightsinglequoted-show-atpt ()
  "Shows hidden LEFTRIGHTSINGLEQUOTED at point. "
  (interactive)
  (ar-th-show 'leftrightsinglequoted))

(defalias 'ar-hide-show-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-hide-show-atpt)
(defun ar-leftrightsinglequoted-hide-show-atpt ()
  "Alternatively hides or shows LEFTRIGHTSINGLEQUOTED at point. "
  (interactive)
  (ar-th-hide-show 'leftrightsinglequoted))

(defalias 'ar-highlight-leftrightsinglequoted-atpt-mode 'ar-leftrightsinglequoted-highlight-atpt-mode)

(defun ar-leftrightsinglequoted-highlight-atpt-mode (&optional no-delimiters)
  "Toggles leftrightsinglequoted-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'leftrightsinglequoted no-delimiters (interactive-p)))

(defalias 'ar-kill-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-kill-atpt)
(defun ar-leftrightsinglequoted-kill-atpt (&optional no-delimiters)
  "Kills LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-kill 'leftrightsinglequoted no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-kill-backward-atpt)
(defun ar-leftrightsinglequoted-kill-backward-atpt (&optional no-delimiters)
  "Kills LEFTRIGHTSINGLEQUOTED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'leftrightsinglequoted no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-leftrightsinglequote-atpt)
;; ;; (defun ar-leftrightsinglequoted-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'leftrightsinglequoted no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-parentize-atpt)
;; ;; (defun ar-leftrightsinglequoted-parentize-atpt (&optional no-delimiters)
;;   "Parentizes LEFTRIGHTSINGLEQUOTED at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'leftrightsinglequoted no-delimiters (interactive-p)))

(defalias 'ar-separate-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-separate-atpt)
(defun ar-leftrightsinglequoted-separate-atpt (&optional no-delimiters)
  "Separates LEFTRIGHTSINGLEQUOTED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'leftrightsinglequoted no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-singlequote-atpt)
;; ;; (defun ar-leftrightsinglequoted-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes LEFTRIGHTSINGLEQUOTED at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'leftrightsinglequoted no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-triplequotedq-atpt)
(defun ar-leftrightsinglequoted-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around leftrightsinglequoted. "
  (interactive "*p")
  (ar-th-triplequotedq 'leftrightsinglequoted no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-triplequotesq-atpt)
(defun ar-leftrightsinglequoted-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around leftrightsinglequoted. "
  (interactive "*p")
  (ar-th-triplequotesq 'leftrightsinglequoted no-delimiters (interactive-p)))

(defun ar-underscore-leftrightsinglequoted-atpt (&optional no-delimiters)
  "Put underscore char around LEFTRIGHTSINGLEQUOTED. "
  (interactive "*p")
  (ar-th-underscore 'leftrightsinglequoted no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequoted-whitespace-atpt 'ar-whitespace-leftrightsinglequoted-atpt)
;; ;; (defun ar-whitespace-leftrightsinglequoted-atpt (&optional no-delimiters)
;;   "Put whitespace char around LEFTRIGHTSINGLEQUOTED. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'leftrightsinglequoted nil t))

(defalias 'ar-forward-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-forward-atpt)
(defun ar-leftrightsinglequoted-forward-atpt (&optional arg)
  "Moves forward over LEFTRIGHTSINGLEQUOTED at point if any, does nothing otherwise.
Returns end position of LEFTRIGHTSINGLEQUOTED "
  (interactive "p")
  (ar-th-forward 'leftrightsinglequoted arg (interactive-p)))

(defalias 'ar-backward-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-backward-atpt)
(defun ar-leftrightsinglequoted-backward-atpt (&optional arg)
  "Moves backward over LEFTRIGHTSINGLEQUOTED before point if any, does nothing otherwise.
Returns beginning position of LEFTRIGHTSINGLEQUOTED "
  (interactive "p")
  (ar-th-backward 'leftrightsinglequoted arg (interactive-p)))

(defalias 'ar-transpose-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-transpose-atpt)
(defun ar-leftrightsinglequoted-transpose-atpt (&optional arg)
  "Transposes LEFTRIGHTSINGLEQUOTED with LEFTRIGHTSINGLEQUOTED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'leftrightsinglequoted arg (interactive-p)))

(defalias 'ar-sort-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-sort-atpt)
(defun ar-leftrightsinglequoted-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts leftrightsinglequoteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'leftrightsinglequoted reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-leftrightsinglequoted-atpt 'ar-leftrightsinglequoted-check-atpt)
(defun ar-leftrightsinglequoted-check-atpt ()
  "Return t if a LEFTRIGHTSINGLEQUOTED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-leftrightsinglequoted-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-leftrightsinglequoted-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-parentized-atpt (&optional arg no-delimiters)
  "Returns parentized at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'parentized arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-parentized-atpt 'ar-parentized-bounds-atpt)
(defun ar-parentized-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of parentized if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'parentized no-delimiters (interactive-p)))

(defun ar-parentized-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position PARENTIZED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'parentized no-delimiters (interactive-p)))

(defun ar-parentized-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'parentized no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-parentized-atpt 'ar-parentized-beginning-atpt)
(defun ar-parentized-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'parentized no-delimiters (interactive-p)))

(defalias 'ar-end-of-parentized-atpt 'ar-parentized-end-atpt)
(defun ar-parentized-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'parentized no-delimiters (interactive-p)))

(defalias 'ar-in-parentized-p-atpt 'ar-parentized-in-p-atpt)
(defun ar-parentized-in-p-atpt (&optional no-delimiters)
  "Returns bounds of PARENTIZED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'parentized no-delimiters (interactive-p)))

(defalias 'ar-length-of-parentized-atpt 'ar-parentized-length-atpt)
(defun ar-parentized-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'parentized no-delimiters (interactive-p)))

(defalias 'ar-copy-parentized-atpt 'ar-parentized-copy-atpt)
(defun ar-parentized-copy-atpt (&optional no-delimiters)
  "Returns a copy of PARENTIZED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'parentized no-delimiters (interactive-p)))

(defalias 'ar-delete-parentized-atpt 'ar-parentized-delete-atpt)
(defun ar-parentized-delete-atpt (&optional arg)
  "Deletes PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-delete 'parentized arg (interactive-p)))

(defalias 'ar-delete-parentized-in-region 'ar-parentized-delete-in-region)
(defun ar-parentized-delete-in-region (beg end)
  "Deletes PARENTIZED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'parentized beg end (interactive-p)))

(defun ar-blok-parentized-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around parentized.
  Returns blok or nil if no PARENTIZED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'parentized no-delimiters (interactive-p)))

(defun ar-doublebackslash-parentized-atpt (&optional no-delimiters)
  "Puts doubled backslashes around PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'parentized no-delimiters (interactive-p)))

(defun ar-doubleslash-parentized-atpt (&optional no-delimiters)
  "Puts doubled slashes around PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'parentized no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-parentized-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'parentized no-delimiters (interactive-p)))

(defun ar-comment-parentized-atpt (&optional no-delimiters)
  "Comments PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-comment 'parentized no-delimiters (interactive-p)))

(defun ar-commatize-parentized-atpt (&optional no-delimiters)
  "Put a comma after PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'parentized no-delimiters (interactive-p)))

(defun ar-quote-parentized-atpt (&optional no-delimiters)
  "Put a singlequote before PARENTIZED at point if any. "
  (interactive "*p")
  (ar-th-quote 'parentized no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-parentized-atpt 'ar-parentized-hyphen-atpt)
;; ;; (defun ar-parentized-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around PARENTIZED at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'parentized no-delimiters (interactive-p)))

(defalias 'ar-mark-parentized-atpt 'ar-parentized-mark-atpt)
(defun ar-parentized-mark-atpt ()
  "Marks PARENTIZED at point if any. "
  (interactive)
  (ar-th-mark 'parentized))

(defalias 'ar-hide-parentized-atpt 'ar-parentized-hide-atpt)
(defun ar-parentized-hide-atpt ()
  "Hides PARENTIZED at point. "
  (interactive)
  (ar-th-hide 'parentized))

(defalias 'ar-show-parentized-atpt 'ar-parentized-show-atpt)
(defun ar-parentized-show-atpt ()
  "Shows hidden PARENTIZED at point. "
  (interactive)
  (ar-th-show 'parentized))

(defalias 'ar-hide-show-parentized-atpt 'ar-parentized-hide-show-atpt)
(defun ar-parentized-hide-show-atpt ()
  "Alternatively hides or shows PARENTIZED at point. "
  (interactive)
  (ar-th-hide-show 'parentized))

(defalias 'ar-highlight-parentized-atpt-mode 'ar-parentized-highlight-atpt-mode)

(defun ar-parentized-highlight-atpt-mode (&optional no-delimiters)
  "Toggles parentized-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'parentized no-delimiters (interactive-p)))

(defalias 'ar-kill-parentized-atpt 'ar-parentized-kill-atpt)
(defun ar-parentized-kill-atpt (&optional no-delimiters)
  "Kills PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-kill 'parentized no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-parentized-atpt 'ar-parentized-kill-backward-atpt)
(defun ar-parentized-kill-backward-atpt (&optional no-delimiters)
  "Kills PARENTIZED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'parentized no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-parentized-atpt 'ar-parentized-leftrightsinglequote-atpt)
;; ;; (defun ar-parentized-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'parentized no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-parentized-atpt 'ar-parentized-parentize-atpt)
;; ;; (defun ar-parentized-parentize-atpt (&optional no-delimiters)
;;   "Parentizes PARENTIZED at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'parentized no-delimiters (interactive-p)))

(defalias 'ar-separate-parentized-atpt 'ar-parentized-separate-atpt)
(defun ar-parentized-separate-atpt (&optional no-delimiters)
  "Separates PARENTIZED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'parentized no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-parentized-atpt 'ar-parentized-singlequote-atpt)
;; ;; (defun ar-parentized-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes PARENTIZED at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'parentized no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-parentized-atpt 'ar-parentized-triplequotedq-atpt)
(defun ar-parentized-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around parentized. "
  (interactive "*p")
  (ar-th-triplequotedq 'parentized no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-parentized-atpt 'ar-parentized-triplequotesq-atpt)
(defun ar-parentized-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around parentized. "
  (interactive "*p")
  (ar-th-triplequotesq 'parentized no-delimiters (interactive-p)))

(defun ar-underscore-parentized-atpt (&optional no-delimiters)
  "Put underscore char around PARENTIZED. "
  (interactive "*p")
  (ar-th-underscore 'parentized no-delimiters (interactive-p)))

;; (defalias 'ar-parentized-whitespace-atpt 'ar-whitespace-parentized-atpt)
;; ;; (defun ar-whitespace-parentized-atpt (&optional no-delimiters)
;;   "Put whitespace char around PARENTIZED. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'parentized nil t))

(defalias 'ar-forward-parentized-atpt 'ar-parentized-forward-atpt)
(defun ar-parentized-forward-atpt (&optional arg)
  "Moves forward over PARENTIZED at point if any, does nothing otherwise.
Returns end position of PARENTIZED "
  (interactive "p")
  (ar-th-forward 'parentized arg (interactive-p)))

(defalias 'ar-backward-parentized-atpt 'ar-parentized-backward-atpt)
(defun ar-parentized-backward-atpt (&optional arg)
  "Moves backward over PARENTIZED before point if any, does nothing otherwise.
Returns beginning position of PARENTIZED "
  (interactive "p")
  (ar-th-backward 'parentized arg (interactive-p)))

(defalias 'ar-transpose-parentized-atpt 'ar-parentized-transpose-atpt)
(defun ar-parentized-transpose-atpt (&optional arg)
  "Transposes PARENTIZED with PARENTIZED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'parentized arg (interactive-p)))

(defalias 'ar-sort-parentized-atpt 'ar-parentized-sort-atpt)
(defun ar-parentized-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts parentizeds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'parentized reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-parentized-atpt 'ar-parentized-check-atpt)
(defun ar-parentized-check-atpt ()
  "Return t if a PARENTIZED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-parentized-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-parentized-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimiters-core: ar-paired-delimited-passiv end

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-rest-list start

(defun ar-greateranglednested-atpt (&optional arg no-delimiters)
  "Returns greateranglednested at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'greateranglednested arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-greateranglednested-atpt 'ar-greateranglednested-bounds-atpt)
(defun ar-greateranglednested-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of greateranglednested if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'greateranglednested no-delimiters (interactive-p)))

(defun ar-greateranglednested-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position GREATERANGLEDNESTED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'greateranglednested no-delimiters (interactive-p)))

(defun ar-greateranglednested-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'greateranglednested no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-greateranglednested-atpt 'ar-greateranglednested-beginning-atpt)
(defun ar-greateranglednested-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'greateranglednested no-delimiters (interactive-p)))

(defalias 'ar-end-of-greateranglednested-atpt 'ar-greateranglednested-end-atpt)
(defun ar-greateranglednested-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'greateranglednested no-delimiters (interactive-p)))

(defalias 'ar-in-greateranglednested-p-atpt 'ar-greateranglednested-in-p-atpt)
(defun ar-greateranglednested-in-p-atpt (&optional no-delimiters)
  "Returns bounds of GREATERANGLEDNESTED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'greateranglednested no-delimiters (interactive-p)))

(defalias 'ar-length-of-greateranglednested-atpt 'ar-greateranglednested-length-atpt)
(defun ar-greateranglednested-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'greateranglednested no-delimiters (interactive-p)))

(defalias 'ar-copy-greateranglednested-atpt 'ar-greateranglednested-copy-atpt)
(defun ar-greateranglednested-copy-atpt (&optional no-delimiters)
  "Returns a copy of GREATERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'greateranglednested no-delimiters (interactive-p)))

(defalias 'ar-delete-greateranglednested-atpt 'ar-greateranglednested-delete-atpt)
(defun ar-greateranglednested-delete-atpt (&optional arg)
  "Deletes GREATERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-delete 'greateranglednested arg (interactive-p)))

(defalias 'ar-delete-greateranglednested-in-region 'ar-greateranglednested-delete-in-region)
(defun ar-greateranglednested-delete-in-region (beg end)
  "Deletes GREATERANGLEDNESTED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'greateranglednested beg end (interactive-p)))

(defun ar-blok-greateranglednested-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around greateranglednested.
  Returns blok or nil if no GREATERANGLEDNESTED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'greateranglednested no-delimiters (interactive-p)))

(defun ar-doublebackslash-greateranglednested-atpt (&optional no-delimiters)
  "Puts doubled backslashes around GREATERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'greateranglednested no-delimiters (interactive-p)))

(defun ar-doubleslash-greateranglednested-atpt (&optional no-delimiters)
  "Puts doubled slashes around GREATERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'greateranglednested no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-greateranglednested-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around GREATERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'greateranglednested no-delimiters (interactive-p)))

(defun ar-comment-greateranglednested-atpt (&optional no-delimiters)
  "Comments GREATERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-comment 'greateranglednested no-delimiters (interactive-p)))

(defun ar-commatize-greateranglednested-atpt (&optional no-delimiters)
  "Put a comma after GREATERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'greateranglednested no-delimiters (interactive-p)))

(defun ar-quote-greateranglednested-atpt (&optional no-delimiters)
  "Put a singlequote before GREATERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-quote 'greateranglednested no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-greateranglednested-atpt 'ar-greateranglednested-hyphen-atpt)
;; ;; (defun ar-greateranglednested-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around GREATERANGLEDNESTED at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'greateranglednested no-delimiters (interactive-p)))

(defalias 'ar-mark-greateranglednested-atpt 'ar-greateranglednested-mark-atpt)
(defun ar-greateranglednested-mark-atpt ()
  "Marks GREATERANGLEDNESTED at point if any. "
  (interactive)
  (ar-th-mark 'greateranglednested))

(defalias 'ar-hide-greateranglednested-atpt 'ar-greateranglednested-hide-atpt)
(defun ar-greateranglednested-hide-atpt ()
  "Hides GREATERANGLEDNESTED at point. "
  (interactive)
  (ar-th-hide 'greateranglednested))

(defalias 'ar-show-greateranglednested-atpt 'ar-greateranglednested-show-atpt)
(defun ar-greateranglednested-show-atpt ()
  "Shows hidden GREATERANGLEDNESTED at point. "
  (interactive)
  (ar-th-show 'greateranglednested))

(defalias 'ar-hide-show-greateranglednested-atpt 'ar-greateranglednested-hide-show-atpt)
(defun ar-greateranglednested-hide-show-atpt ()
  "Alternatively hides or shows GREATERANGLEDNESTED at point. "
  (interactive)
  (ar-th-hide-show 'greateranglednested))

(defalias 'ar-highlight-greateranglednested-atpt-mode 'ar-greateranglednested-highlight-atpt-mode)

(defun ar-greateranglednested-highlight-atpt-mode (&optional no-delimiters)
  "Toggles greateranglednested-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'greateranglednested no-delimiters (interactive-p)))

(defalias 'ar-kill-greateranglednested-atpt 'ar-greateranglednested-kill-atpt)
(defun ar-greateranglednested-kill-atpt (&optional no-delimiters)
  "Kills GREATERANGLEDNESTED at point if any. "
  (interactive "*P")
  (ar-th-kill 'greateranglednested no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-greateranglednested-atpt 'ar-greateranglednested-kill-backward-atpt)
(defun ar-greateranglednested-kill-backward-atpt (&optional no-delimiters)
  "Kills GREATERANGLEDNESTED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'greateranglednested no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-greateranglednested-atpt 'ar-greateranglednested-leftrightsinglequote-atpt)
;; ;; (defun ar-greateranglednested-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'greateranglednested no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-greateranglednested-atpt 'ar-greateranglednested-parentize-atpt)
;; ;; (defun ar-greateranglednested-parentize-atpt (&optional no-delimiters)
;;   "Parentizes GREATERANGLEDNESTED at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'greateranglednested no-delimiters (interactive-p)))

(defalias 'ar-separate-greateranglednested-atpt 'ar-greateranglednested-separate-atpt)
(defun ar-greateranglednested-separate-atpt (&optional no-delimiters)
  "Separates GREATERANGLEDNESTED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'greateranglednested no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-greateranglednested-atpt 'ar-greateranglednested-singlequote-atpt)
;; ;; (defun ar-greateranglednested-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes GREATERANGLEDNESTED at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'greateranglednested no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-greateranglednested-atpt 'ar-greateranglednested-triplequotedq-atpt)
(defun ar-greateranglednested-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around greateranglednested. "
  (interactive "*p")
  (ar-th-triplequotedq 'greateranglednested no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-greateranglednested-atpt 'ar-greateranglednested-triplequotesq-atpt)
(defun ar-greateranglednested-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around greateranglednested. "
  (interactive "*p")
  (ar-th-triplequotesq 'greateranglednested no-delimiters (interactive-p)))

(defun ar-underscore-greateranglednested-atpt (&optional no-delimiters)
  "Put underscore char around GREATERANGLEDNESTED. "
  (interactive "*p")
  (ar-th-underscore 'greateranglednested no-delimiters (interactive-p)))

;; (defalias 'ar-greateranglednested-whitespace-atpt 'ar-whitespace-greateranglednested-atpt)
;; ;; (defun ar-whitespace-greateranglednested-atpt (&optional no-delimiters)
;;   "Put whitespace char around GREATERANGLEDNESTED. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'greateranglednested nil t))

(defalias 'ar-forward-greateranglednested-atpt 'ar-greateranglednested-forward-atpt)
(defun ar-greateranglednested-forward-atpt (&optional arg)
  "Moves forward over GREATERANGLEDNESTED at point if any, does nothing otherwise.
Returns end position of GREATERANGLEDNESTED "
  (interactive "p")
  (ar-th-forward 'greateranglednested arg (interactive-p)))

(defalias 'ar-backward-greateranglednested-atpt 'ar-greateranglednested-backward-atpt)
(defun ar-greateranglednested-backward-atpt (&optional arg)
  "Moves backward over GREATERANGLEDNESTED before point if any, does nothing otherwise.
Returns beginning position of GREATERANGLEDNESTED "
  (interactive "p")
  (ar-th-backward 'greateranglednested arg (interactive-p)))

(defalias 'ar-transpose-greateranglednested-atpt 'ar-greateranglednested-transpose-atpt)
(defun ar-greateranglednested-transpose-atpt (&optional arg)
  "Transposes GREATERANGLEDNESTED with GREATERANGLEDNESTED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'greateranglednested arg (interactive-p)))

(defalias 'ar-sort-greateranglednested-atpt 'ar-greateranglednested-sort-atpt)
(defun ar-greateranglednested-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts greateranglednesteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'greateranglednested reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-greateranglednested-atpt 'ar-greateranglednested-check-atpt)
(defun ar-greateranglednested-check-atpt ()
  "Return t if a GREATERANGLEDNESTED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-greateranglednested-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-greateranglednested-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-lesseranglednested-atpt (&optional arg no-delimiters)
  "Returns lesseranglednested at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'lesseranglednested arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-lesseranglednested-atpt 'ar-lesseranglednested-bounds-atpt)
(defun ar-lesseranglednested-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of lesseranglednested if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lesseranglednested no-delimiters (interactive-p)))

(defun ar-lesseranglednested-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position LESSERANGLEDNESTED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'lesseranglednested no-delimiters (interactive-p)))

(defun ar-lesseranglednested-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'lesseranglednested no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-lesseranglednested-atpt 'ar-lesseranglednested-beginning-atpt)
(defun ar-lesseranglednested-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'lesseranglednested no-delimiters (interactive-p)))

(defalias 'ar-end-of-lesseranglednested-atpt 'ar-lesseranglednested-end-atpt)
(defun ar-lesseranglednested-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'lesseranglednested no-delimiters (interactive-p)))

(defalias 'ar-in-lesseranglednested-p-atpt 'ar-lesseranglednested-in-p-atpt)
(defun ar-lesseranglednested-in-p-atpt (&optional no-delimiters)
  "Returns bounds of LESSERANGLEDNESTED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'lesseranglednested no-delimiters (interactive-p)))

(defalias 'ar-length-of-lesseranglednested-atpt 'ar-lesseranglednested-length-atpt)
(defun ar-lesseranglednested-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'lesseranglednested no-delimiters (interactive-p)))

(defalias 'ar-copy-lesseranglednested-atpt 'ar-lesseranglednested-copy-atpt)
(defun ar-lesseranglednested-copy-atpt (&optional no-delimiters)
  "Returns a copy of LESSERANGLEDNESTED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'lesseranglednested no-delimiters (interactive-p)))

(defalias 'ar-delete-lesseranglednested-atpt 'ar-lesseranglednested-delete-atpt)
(defun ar-lesseranglednested-delete-atpt (&optional arg)
  "Deletes LESSERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-delete 'lesseranglednested arg (interactive-p)))

(defalias 'ar-delete-lesseranglednested-in-region 'ar-lesseranglednested-delete-in-region)
(defun ar-lesseranglednested-delete-in-region (beg end)
  "Deletes LESSERANGLEDNESTED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'lesseranglednested beg end (interactive-p)))

(defun ar-blok-lesseranglednested-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around lesseranglednested.
  Returns blok or nil if no LESSERANGLEDNESTED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'lesseranglednested no-delimiters (interactive-p)))

(defun ar-doublebackslash-lesseranglednested-atpt (&optional no-delimiters)
  "Puts doubled backslashes around LESSERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'lesseranglednested no-delimiters (interactive-p)))

(defun ar-doubleslash-lesseranglednested-atpt (&optional no-delimiters)
  "Puts doubled slashes around LESSERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'lesseranglednested no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-lesseranglednested-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around LESSERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'lesseranglednested no-delimiters (interactive-p)))

(defun ar-comment-lesseranglednested-atpt (&optional no-delimiters)
  "Comments LESSERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-comment 'lesseranglednested no-delimiters (interactive-p)))

(defun ar-commatize-lesseranglednested-atpt (&optional no-delimiters)
  "Put a comma after LESSERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'lesseranglednested no-delimiters (interactive-p)))

(defun ar-quote-lesseranglednested-atpt (&optional no-delimiters)
  "Put a singlequote before LESSERANGLEDNESTED at point if any. "
  (interactive "*p")
  (ar-th-quote 'lesseranglednested no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-lesseranglednested-atpt 'ar-lesseranglednested-hyphen-atpt)
;; ;; (defun ar-lesseranglednested-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around LESSERANGLEDNESTED at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'lesseranglednested no-delimiters (interactive-p)))

(defalias 'ar-mark-lesseranglednested-atpt 'ar-lesseranglednested-mark-atpt)
(defun ar-lesseranglednested-mark-atpt ()
  "Marks LESSERANGLEDNESTED at point if any. "
  (interactive)
  (ar-th-mark 'lesseranglednested))

(defalias 'ar-hide-lesseranglednested-atpt 'ar-lesseranglednested-hide-atpt)
(defun ar-lesseranglednested-hide-atpt ()
  "Hides LESSERANGLEDNESTED at point. "
  (interactive)
  (ar-th-hide 'lesseranglednested))

(defalias 'ar-show-lesseranglednested-atpt 'ar-lesseranglednested-show-atpt)
(defun ar-lesseranglednested-show-atpt ()
  "Shows hidden LESSERANGLEDNESTED at point. "
  (interactive)
  (ar-th-show 'lesseranglednested))

(defalias 'ar-hide-show-lesseranglednested-atpt 'ar-lesseranglednested-hide-show-atpt)
(defun ar-lesseranglednested-hide-show-atpt ()
  "Alternatively hides or shows LESSERANGLEDNESTED at point. "
  (interactive)
  (ar-th-hide-show 'lesseranglednested))

(defalias 'ar-highlight-lesseranglednested-atpt-mode 'ar-lesseranglednested-highlight-atpt-mode)

(defun ar-lesseranglednested-highlight-atpt-mode (&optional no-delimiters)
  "Toggles lesseranglednested-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'lesseranglednested no-delimiters (interactive-p)))

(defalias 'ar-kill-lesseranglednested-atpt 'ar-lesseranglednested-kill-atpt)
(defun ar-lesseranglednested-kill-atpt (&optional no-delimiters)
  "Kills LESSERANGLEDNESTED at point if any. "
  (interactive "*P")
  (ar-th-kill 'lesseranglednested no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-lesseranglednested-atpt 'ar-lesseranglednested-kill-backward-atpt)
(defun ar-lesseranglednested-kill-backward-atpt (&optional no-delimiters)
  "Kills LESSERANGLEDNESTED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'lesseranglednested no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-lesseranglednested-atpt 'ar-lesseranglednested-leftrightsinglequote-atpt)
;; ;; (defun ar-lesseranglednested-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'lesseranglednested no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-lesseranglednested-atpt 'ar-lesseranglednested-parentize-atpt)
;; ;; (defun ar-lesseranglednested-parentize-atpt (&optional no-delimiters)
;;   "Parentizes LESSERANGLEDNESTED at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'lesseranglednested no-delimiters (interactive-p)))

(defalias 'ar-separate-lesseranglednested-atpt 'ar-lesseranglednested-separate-atpt)
(defun ar-lesseranglednested-separate-atpt (&optional no-delimiters)
  "Separates LESSERANGLEDNESTED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'lesseranglednested no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-lesseranglednested-atpt 'ar-lesseranglednested-singlequote-atpt)
;; ;; (defun ar-lesseranglednested-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes LESSERANGLEDNESTED at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'lesseranglednested no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-lesseranglednested-atpt 'ar-lesseranglednested-triplequotedq-atpt)
(defun ar-lesseranglednested-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around lesseranglednested. "
  (interactive "*p")
  (ar-th-triplequotedq 'lesseranglednested no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-lesseranglednested-atpt 'ar-lesseranglednested-triplequotesq-atpt)
(defun ar-lesseranglednested-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around lesseranglednested. "
  (interactive "*p")
  (ar-th-triplequotesq 'lesseranglednested no-delimiters (interactive-p)))

(defun ar-underscore-lesseranglednested-atpt (&optional no-delimiters)
  "Put underscore char around LESSERANGLEDNESTED. "
  (interactive "*p")
  (ar-th-underscore 'lesseranglednested no-delimiters (interactive-p)))

;; (defalias 'ar-lesseranglednested-whitespace-atpt 'ar-whitespace-lesseranglednested-atpt)
;; ;; (defun ar-whitespace-lesseranglednested-atpt (&optional no-delimiters)
;;   "Put whitespace char around LESSERANGLEDNESTED. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'lesseranglednested nil t))

(defalias 'ar-forward-lesseranglednested-atpt 'ar-lesseranglednested-forward-atpt)
(defun ar-lesseranglednested-forward-atpt (&optional arg)
  "Moves forward over LESSERANGLEDNESTED at point if any, does nothing otherwise.
Returns end position of LESSERANGLEDNESTED "
  (interactive "p")
  (ar-th-forward 'lesseranglednested arg (interactive-p)))

(defalias 'ar-backward-lesseranglednested-atpt 'ar-lesseranglednested-backward-atpt)
(defun ar-lesseranglednested-backward-atpt (&optional arg)
  "Moves backward over LESSERANGLEDNESTED before point if any, does nothing otherwise.
Returns beginning position of LESSERANGLEDNESTED "
  (interactive "p")
  (ar-th-backward 'lesseranglednested arg (interactive-p)))

(defalias 'ar-transpose-lesseranglednested-atpt 'ar-lesseranglednested-transpose-atpt)
(defun ar-lesseranglednested-transpose-atpt (&optional arg)
  "Transposes LESSERANGLEDNESTED with LESSERANGLEDNESTED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'lesseranglednested arg (interactive-p)))

(defalias 'ar-sort-lesseranglednested-atpt 'ar-lesseranglednested-sort-atpt)
(defun ar-lesseranglednested-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lesseranglednesteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'lesseranglednested reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-lesseranglednested-atpt 'ar-lesseranglednested-check-atpt)
(defun ar-lesseranglednested-check-atpt ()
  "Return t if a LESSERANGLEDNESTED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-lesseranglednested-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-lesseranglednested-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-buffer-atpt (&optional arg no-delimiters)
  "Returns buffer at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'buffer arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-buffer-atpt 'ar-buffer-bounds-atpt)
(defun ar-buffer-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of buffer if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'buffer no-delimiters (interactive-p)))

(defun ar-buffer-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BUFFER at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'buffer no-delimiters (interactive-p)))

(defun ar-buffer-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'buffer no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-buffer-atpt 'ar-buffer-beginning-atpt)
(defun ar-buffer-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'buffer no-delimiters (interactive-p)))

(defalias 'ar-end-of-buffer-atpt 'ar-buffer-end-atpt)
(defun ar-buffer-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'buffer no-delimiters (interactive-p)))

(defalias 'ar-in-buffer-p-atpt 'ar-buffer-in-p-atpt)
(defun ar-buffer-in-p-atpt (&optional no-delimiters)
  "Returns bounds of BUFFER at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'buffer no-delimiters (interactive-p)))

(defalias 'ar-length-of-buffer-atpt 'ar-buffer-length-atpt)
(defun ar-buffer-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'buffer no-delimiters (interactive-p)))

(defalias 'ar-copy-buffer-atpt 'ar-buffer-copy-atpt)
(defun ar-buffer-copy-atpt (&optional no-delimiters)
  "Returns a copy of BUFFER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'buffer no-delimiters (interactive-p)))

(defalias 'ar-delete-buffer-atpt 'ar-buffer-delete-atpt)
(defun ar-buffer-delete-atpt (&optional arg)
  "Deletes BUFFER at point if any. "
  (interactive "*p")
  (ar-th-delete 'buffer arg (interactive-p)))

(defalias 'ar-delete-buffer-in-region 'ar-buffer-delete-in-region)
(defun ar-buffer-delete-in-region (beg end)
  "Deletes BUFFER at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'buffer beg end (interactive-p)))

(defun ar-blok-buffer-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around buffer.
  Returns blok or nil if no BUFFER at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'buffer no-delimiters (interactive-p)))

(defun ar-doublebackslash-buffer-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BUFFER at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'buffer no-delimiters (interactive-p)))

(defun ar-doubleslash-buffer-atpt (&optional no-delimiters)
  "Puts doubled slashes around BUFFER at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'buffer no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-buffer-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BUFFER at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'buffer no-delimiters (interactive-p)))

(defun ar-comment-buffer-atpt (&optional no-delimiters)
  "Comments BUFFER at point if any. "
  (interactive "*p")
  (ar-th-comment 'buffer no-delimiters (interactive-p)))

(defun ar-commatize-buffer-atpt (&optional no-delimiters)
  "Put a comma after BUFFER at point if any. "
  (interactive "*p")
  (ar-th-commatize 'buffer no-delimiters (interactive-p)))

(defun ar-quote-buffer-atpt (&optional no-delimiters)
  "Put a singlequote before BUFFER at point if any. "
  (interactive "*p")
  (ar-th-quote 'buffer no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-buffer-atpt 'ar-buffer-hyphen-atpt)
;; ;; (defun ar-buffer-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around BUFFER at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'buffer no-delimiters (interactive-p)))

(defalias 'ar-mark-buffer-atpt 'ar-buffer-mark-atpt)
(defun ar-buffer-mark-atpt ()
  "Marks BUFFER at point if any. "
  (interactive)
  (ar-th-mark 'buffer))

(defalias 'ar-hide-buffer-atpt 'ar-buffer-hide-atpt)
(defun ar-buffer-hide-atpt ()
  "Hides BUFFER at point. "
  (interactive)
  (ar-th-hide 'buffer))

(defalias 'ar-show-buffer-atpt 'ar-buffer-show-atpt)
(defun ar-buffer-show-atpt ()
  "Shows hidden BUFFER at point. "
  (interactive)
  (ar-th-show 'buffer))

(defalias 'ar-hide-show-buffer-atpt 'ar-buffer-hide-show-atpt)
(defun ar-buffer-hide-show-atpt ()
  "Alternatively hides or shows BUFFER at point. "
  (interactive)
  (ar-th-hide-show 'buffer))

(defalias 'ar-highlight-buffer-atpt-mode 'ar-buffer-highlight-atpt-mode)

(defun ar-buffer-highlight-atpt-mode (&optional no-delimiters)
  "Toggles buffer-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'buffer no-delimiters (interactive-p)))

(defalias 'ar-kill-buffer-atpt 'ar-buffer-kill-atpt)
(defun ar-buffer-kill-atpt (&optional no-delimiters)
  "Kills BUFFER at point if any. "
  (interactive "*P")
  (ar-th-kill 'buffer no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-buffer-atpt 'ar-buffer-kill-backward-atpt)
(defun ar-buffer-kill-backward-atpt (&optional no-delimiters)
  "Kills BUFFER at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'buffer no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-buffer-atpt 'ar-buffer-leftrightsinglequote-atpt)
;; ;; (defun ar-buffer-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'buffer no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-buffer-atpt 'ar-buffer-parentize-atpt)
;; ;; (defun ar-buffer-parentize-atpt (&optional no-delimiters)
;;   "Parentizes BUFFER at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'buffer no-delimiters (interactive-p)))

(defalias 'ar-separate-buffer-atpt 'ar-buffer-separate-atpt)
(defun ar-buffer-separate-atpt (&optional no-delimiters)
  "Separates BUFFER at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'buffer no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-buffer-atpt 'ar-buffer-singlequote-atpt)
;; ;; (defun ar-buffer-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes BUFFER at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'buffer no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-buffer-atpt 'ar-buffer-triplequotedq-atpt)
(defun ar-buffer-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around buffer. "
  (interactive "*p")
  (ar-th-triplequotedq 'buffer no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-buffer-atpt 'ar-buffer-triplequotesq-atpt)
(defun ar-buffer-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around buffer. "
  (interactive "*p")
  (ar-th-triplequotesq 'buffer no-delimiters (interactive-p)))

(defun ar-underscore-buffer-atpt (&optional no-delimiters)
  "Put underscore char around BUFFER. "
  (interactive "*p")
  (ar-th-underscore 'buffer no-delimiters (interactive-p)))

;; (defalias 'ar-buffer-whitespace-atpt 'ar-whitespace-buffer-atpt)
;; ;; (defun ar-whitespace-buffer-atpt (&optional no-delimiters)
;;   "Put whitespace char around BUFFER. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'buffer nil t))

(defalias 'ar-forward-buffer-atpt 'ar-buffer-forward-atpt)
(defun ar-buffer-forward-atpt (&optional arg)
  "Moves forward over BUFFER at point if any, does nothing otherwise.
Returns end position of BUFFER "
  (interactive "p")
  (ar-th-forward 'buffer arg (interactive-p)))

(defalias 'ar-backward-buffer-atpt 'ar-buffer-backward-atpt)
(defun ar-buffer-backward-atpt (&optional arg)
  "Moves backward over BUFFER before point if any, does nothing otherwise.
Returns beginning position of BUFFER "
  (interactive "p")
  (ar-th-backward 'buffer arg (interactive-p)))

(defalias 'ar-transpose-buffer-atpt 'ar-buffer-transpose-atpt)
(defun ar-buffer-transpose-atpt (&optional arg)
  "Transposes BUFFER with BUFFER before point if any. "
  (interactive "*p")
  (ar-th-transpose 'buffer arg (interactive-p)))

(defalias 'ar-sort-buffer-atpt 'ar-buffer-sort-atpt)
(defun ar-buffer-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts buffers in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'buffer reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-buffer-atpt 'ar-buffer-check-atpt)
(defun ar-buffer-check-atpt ()
  "Return t if a BUFFER at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-buffer-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-buffer-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-comment-atpt (&optional arg no-delimiters)
  "Returns comment at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'comment arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-comment-atpt 'ar-comment-bounds-atpt)
(defun ar-comment-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of comment if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'comment no-delimiters (interactive-p)))

(defun ar-comment-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position COMMENT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'comment no-delimiters (interactive-p)))

(defun ar-comment-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'comment no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-comment-atpt 'ar-comment-beginning-atpt)
(defun ar-comment-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'comment no-delimiters (interactive-p)))

(defalias 'ar-end-of-comment-atpt 'ar-comment-end-atpt)
(defun ar-comment-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'comment no-delimiters (interactive-p)))

(defalias 'ar-in-comment-p-atpt 'ar-comment-in-p-atpt)
(defun ar-comment-in-p-atpt (&optional no-delimiters)
  "Returns bounds of COMMENT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'comment no-delimiters (interactive-p)))

(defalias 'ar-length-of-comment-atpt 'ar-comment-length-atpt)
(defun ar-comment-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'comment no-delimiters (interactive-p)))

(defalias 'ar-copy-comment-atpt 'ar-comment-copy-atpt)
(defun ar-comment-copy-atpt (&optional no-delimiters)
  "Returns a copy of COMMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'comment no-delimiters (interactive-p)))

(defalias 'ar-delete-comment-atpt 'ar-comment-delete-atpt)
(defun ar-comment-delete-atpt (&optional arg)
  "Deletes COMMENT at point if any. "
  (interactive "*p")
  (ar-th-delete 'comment arg (interactive-p)))

(defalias 'ar-delete-comment-in-region 'ar-comment-delete-in-region)
(defun ar-comment-delete-in-region (beg end)
  "Deletes COMMENT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'comment beg end (interactive-p)))

(defun ar-blok-comment-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around comment.
  Returns blok or nil if no COMMENT at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'comment no-delimiters (interactive-p)))

(defun ar-doublebackslash-comment-atpt (&optional no-delimiters)
  "Puts doubled backslashes around COMMENT at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'comment no-delimiters (interactive-p)))

(defun ar-doubleslash-comment-atpt (&optional no-delimiters)
  "Puts doubled slashes around COMMENT at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'comment no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-comment-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around COMMENT at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'comment no-delimiters (interactive-p)))

(defun ar-comment-comment-atpt (&optional no-delimiters)
  "Comments COMMENT at point if any. "
  (interactive "*p")
  (ar-th-comment 'comment no-delimiters (interactive-p)))

(defun ar-commatize-comment-atpt (&optional no-delimiters)
  "Put a comma after COMMENT at point if any. "
  (interactive "*p")
  (ar-th-commatize 'comment no-delimiters (interactive-p)))

(defun ar-quote-comment-atpt (&optional no-delimiters)
  "Put a singlequote before COMMENT at point if any. "
  (interactive "*p")
  (ar-th-quote 'comment no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-comment-atpt 'ar-comment-hyphen-atpt)
;; ;; (defun ar-comment-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around COMMENT at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'comment no-delimiters (interactive-p)))

(defalias 'ar-mark-comment-atpt 'ar-comment-mark-atpt)
(defun ar-comment-mark-atpt ()
  "Marks COMMENT at point if any. "
  (interactive)
  (ar-th-mark 'comment))

(defalias 'ar-hide-comment-atpt 'ar-comment-hide-atpt)
(defun ar-comment-hide-atpt ()
  "Hides COMMENT at point. "
  (interactive)
  (ar-th-hide 'comment))

(defalias 'ar-show-comment-atpt 'ar-comment-show-atpt)
(defun ar-comment-show-atpt ()
  "Shows hidden COMMENT at point. "
  (interactive)
  (ar-th-show 'comment))

(defalias 'ar-hide-show-comment-atpt 'ar-comment-hide-show-atpt)
(defun ar-comment-hide-show-atpt ()
  "Alternatively hides or shows COMMENT at point. "
  (interactive)
  (ar-th-hide-show 'comment))

(defalias 'ar-highlight-comment-atpt-mode 'ar-comment-highlight-atpt-mode)

(defun ar-comment-highlight-atpt-mode (&optional no-delimiters)
  "Toggles comment-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'comment no-delimiters (interactive-p)))

(defalias 'ar-kill-comment-atpt 'ar-comment-kill-atpt)
(defun ar-comment-kill-atpt (&optional no-delimiters)
  "Kills COMMENT at point if any. "
  (interactive "*P")
  (ar-th-kill 'comment no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-comment-atpt 'ar-comment-kill-backward-atpt)
(defun ar-comment-kill-backward-atpt (&optional no-delimiters)
  "Kills COMMENT at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'comment no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-comment-atpt 'ar-comment-leftrightsinglequote-atpt)
;; ;; (defun ar-comment-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'comment no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-comment-atpt 'ar-comment-parentize-atpt)
;; ;; (defun ar-comment-parentize-atpt (&optional no-delimiters)
;;   "Parentizes COMMENT at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'comment no-delimiters (interactive-p)))

(defalias 'ar-separate-comment-atpt 'ar-comment-separate-atpt)
(defun ar-comment-separate-atpt (&optional no-delimiters)
  "Separates COMMENT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'comment no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-comment-atpt 'ar-comment-singlequote-atpt)
;; ;; (defun ar-comment-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes COMMENT at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'comment no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-comment-atpt 'ar-comment-triplequotedq-atpt)
(defun ar-comment-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around comment. "
  (interactive "*p")
  (ar-th-triplequotedq 'comment no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-comment-atpt 'ar-comment-triplequotesq-atpt)
(defun ar-comment-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around comment. "
  (interactive "*p")
  (ar-th-triplequotesq 'comment no-delimiters (interactive-p)))

(defun ar-underscore-comment-atpt (&optional no-delimiters)
  "Put underscore char around COMMENT. "
  (interactive "*p")
  (ar-th-underscore 'comment no-delimiters (interactive-p)))

;; (defalias 'ar-comment-whitespace-atpt 'ar-whitespace-comment-atpt)
;; ;; (defun ar-whitespace-comment-atpt (&optional no-delimiters)
;;   "Put whitespace char around COMMENT. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'comment nil t))

(defalias 'ar-forward-comment-atpt 'ar-comment-forward-atpt)
(defun ar-comment-forward-atpt (&optional arg)
  "Moves forward over COMMENT at point if any, does nothing otherwise.
Returns end position of COMMENT "
  (interactive "p")
  (ar-th-forward 'comment arg (interactive-p)))

(defalias 'ar-backward-comment-atpt 'ar-comment-backward-atpt)
(defun ar-comment-backward-atpt (&optional arg)
  "Moves backward over COMMENT before point if any, does nothing otherwise.
Returns beginning position of COMMENT "
  (interactive "p")
  (ar-th-backward 'comment arg (interactive-p)))

(defalias 'ar-transpose-comment-atpt 'ar-comment-transpose-atpt)
(defun ar-comment-transpose-atpt (&optional arg)
  "Transposes COMMENT with COMMENT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'comment arg (interactive-p)))

(defalias 'ar-sort-comment-atpt 'ar-comment-sort-atpt)
(defun ar-comment-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts comments in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'comment reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-comment-atpt 'ar-comment-check-atpt)
(defun ar-comment-check-atpt ()
  "Return t if a COMMENT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-comment-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-comment-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-csv-atpt (&optional arg no-delimiters)
  "Returns csv at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'csv arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-csv-atpt 'ar-csv-bounds-atpt)
(defun ar-csv-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of csv if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'csv no-delimiters (interactive-p)))

(defun ar-csv-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position CSV at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'csv no-delimiters (interactive-p)))

(defun ar-csv-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'csv no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-csv-atpt 'ar-csv-beginning-atpt)
(defun ar-csv-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'csv no-delimiters (interactive-p)))

(defalias 'ar-end-of-csv-atpt 'ar-csv-end-atpt)
(defun ar-csv-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'csv no-delimiters (interactive-p)))

(defalias 'ar-in-csv-p-atpt 'ar-csv-in-p-atpt)
(defun ar-csv-in-p-atpt (&optional no-delimiters)
  "Returns bounds of CSV at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'csv no-delimiters (interactive-p)))

(defalias 'ar-length-of-csv-atpt 'ar-csv-length-atpt)
(defun ar-csv-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'csv no-delimiters (interactive-p)))

(defalias 'ar-copy-csv-atpt 'ar-csv-copy-atpt)
(defun ar-csv-copy-atpt (&optional no-delimiters)
  "Returns a copy of CSV at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'csv no-delimiters (interactive-p)))

(defalias 'ar-delete-csv-atpt 'ar-csv-delete-atpt)
(defun ar-csv-delete-atpt (&optional arg)
  "Deletes CSV at point if any. "
  (interactive "*p")
  (ar-th-delete 'csv arg (interactive-p)))

(defalias 'ar-delete-csv-in-region 'ar-csv-delete-in-region)
(defun ar-csv-delete-in-region (beg end)
  "Deletes CSV at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'csv beg end (interactive-p)))

(defun ar-blok-csv-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around csv.
  Returns blok or nil if no CSV at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'csv no-delimiters (interactive-p)))

(defun ar-doublebackslash-csv-atpt (&optional no-delimiters)
  "Puts doubled backslashes around CSV at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'csv no-delimiters (interactive-p)))

(defun ar-doubleslash-csv-atpt (&optional no-delimiters)
  "Puts doubled slashes around CSV at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'csv no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-csv-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around CSV at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'csv no-delimiters (interactive-p)))

(defun ar-comment-csv-atpt (&optional no-delimiters)
  "Comments CSV at point if any. "
  (interactive "*p")
  (ar-th-comment 'csv no-delimiters (interactive-p)))

(defun ar-commatize-csv-atpt (&optional no-delimiters)
  "Put a comma after CSV at point if any. "
  (interactive "*p")
  (ar-th-commatize 'csv no-delimiters (interactive-p)))

(defun ar-quote-csv-atpt (&optional no-delimiters)
  "Put a singlequote before CSV at point if any. "
  (interactive "*p")
  (ar-th-quote 'csv no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-csv-atpt 'ar-csv-hyphen-atpt)
;; ;; (defun ar-csv-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around CSV at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'csv no-delimiters (interactive-p)))

(defalias 'ar-mark-csv-atpt 'ar-csv-mark-atpt)
(defun ar-csv-mark-atpt ()
  "Marks CSV at point if any. "
  (interactive)
  (ar-th-mark 'csv))

(defalias 'ar-hide-csv-atpt 'ar-csv-hide-atpt)
(defun ar-csv-hide-atpt ()
  "Hides CSV at point. "
  (interactive)
  (ar-th-hide 'csv))

(defalias 'ar-show-csv-atpt 'ar-csv-show-atpt)
(defun ar-csv-show-atpt ()
  "Shows hidden CSV at point. "
  (interactive)
  (ar-th-show 'csv))

(defalias 'ar-hide-show-csv-atpt 'ar-csv-hide-show-atpt)
(defun ar-csv-hide-show-atpt ()
  "Alternatively hides or shows CSV at point. "
  (interactive)
  (ar-th-hide-show 'csv))

(defalias 'ar-highlight-csv-atpt-mode 'ar-csv-highlight-atpt-mode)

(defun ar-csv-highlight-atpt-mode (&optional no-delimiters)
  "Toggles csv-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'csv no-delimiters (interactive-p)))

(defalias 'ar-kill-csv-atpt 'ar-csv-kill-atpt)
(defun ar-csv-kill-atpt (&optional no-delimiters)
  "Kills CSV at point if any. "
  (interactive "*P")
  (ar-th-kill 'csv no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-csv-atpt 'ar-csv-kill-backward-atpt)
(defun ar-csv-kill-backward-atpt (&optional no-delimiters)
  "Kills CSV at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'csv no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-csv-atpt 'ar-csv-leftrightsinglequote-atpt)
;; ;; (defun ar-csv-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'csv no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-csv-atpt 'ar-csv-parentize-atpt)
;; ;; (defun ar-csv-parentize-atpt (&optional no-delimiters)
;;   "Parentizes CSV at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'csv no-delimiters (interactive-p)))

(defalias 'ar-separate-csv-atpt 'ar-csv-separate-atpt)
(defun ar-csv-separate-atpt (&optional no-delimiters)
  "Separates CSV at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'csv no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-csv-atpt 'ar-csv-singlequote-atpt)
;; ;; (defun ar-csv-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes CSV at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'csv no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-csv-atpt 'ar-csv-triplequotedq-atpt)
(defun ar-csv-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around csv. "
  (interactive "*p")
  (ar-th-triplequotedq 'csv no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-csv-atpt 'ar-csv-triplequotesq-atpt)
(defun ar-csv-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around csv. "
  (interactive "*p")
  (ar-th-triplequotesq 'csv no-delimiters (interactive-p)))

(defun ar-underscore-csv-atpt (&optional no-delimiters)
  "Put underscore char around CSV. "
  (interactive "*p")
  (ar-th-underscore 'csv no-delimiters (interactive-p)))

;; (defalias 'ar-csv-whitespace-atpt 'ar-whitespace-csv-atpt)
;; ;; (defun ar-whitespace-csv-atpt (&optional no-delimiters)
;;   "Put whitespace char around CSV. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'csv nil t))

(defalias 'ar-forward-csv-atpt 'ar-csv-forward-atpt)
(defun ar-csv-forward-atpt (&optional arg)
  "Moves forward over CSV at point if any, does nothing otherwise.
Returns end position of CSV "
  (interactive "p")
  (ar-th-forward 'csv arg (interactive-p)))

(defalias 'ar-backward-csv-atpt 'ar-csv-backward-atpt)
(defun ar-csv-backward-atpt (&optional arg)
  "Moves backward over CSV before point if any, does nothing otherwise.
Returns beginning position of CSV "
  (interactive "p")
  (ar-th-backward 'csv arg (interactive-p)))

(defalias 'ar-transpose-csv-atpt 'ar-csv-transpose-atpt)
(defun ar-csv-transpose-atpt (&optional arg)
  "Transposes CSV with CSV before point if any. "
  (interactive "*p")
  (ar-th-transpose 'csv arg (interactive-p)))

(defalias 'ar-sort-csv-atpt 'ar-csv-sort-atpt)
(defun ar-csv-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts csvs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'csv reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-csv-atpt 'ar-csv-check-atpt)
(defun ar-csv-check-atpt ()
  "Return t if a CSV at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-csv-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-csv-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-date-atpt (&optional arg no-delimiters)
  "Returns date at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'date arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-date-atpt 'ar-date-bounds-atpt)
(defun ar-date-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of date if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'date no-delimiters (interactive-p)))

(defun ar-date-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DATE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'date no-delimiters (interactive-p)))

(defun ar-date-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'date no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-date-atpt 'ar-date-beginning-atpt)
(defun ar-date-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'date no-delimiters (interactive-p)))

(defalias 'ar-end-of-date-atpt 'ar-date-end-atpt)
(defun ar-date-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'date no-delimiters (interactive-p)))

(defalias 'ar-in-date-p-atpt 'ar-date-in-p-atpt)
(defun ar-date-in-p-atpt (&optional no-delimiters)
  "Returns bounds of DATE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'date no-delimiters (interactive-p)))

(defalias 'ar-length-of-date-atpt 'ar-date-length-atpt)
(defun ar-date-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'date no-delimiters (interactive-p)))

(defalias 'ar-copy-date-atpt 'ar-date-copy-atpt)
(defun ar-date-copy-atpt (&optional no-delimiters)
  "Returns a copy of DATE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'date no-delimiters (interactive-p)))

(defalias 'ar-delete-date-atpt 'ar-date-delete-atpt)
(defun ar-date-delete-atpt (&optional arg)
  "Deletes DATE at point if any. "
  (interactive "*p")
  (ar-th-delete 'date arg (interactive-p)))

(defalias 'ar-delete-date-in-region 'ar-date-delete-in-region)
(defun ar-date-delete-in-region (beg end)
  "Deletes DATE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'date beg end (interactive-p)))

(defun ar-blok-date-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around date.
  Returns blok or nil if no DATE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'date no-delimiters (interactive-p)))

(defun ar-doublebackslash-date-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DATE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'date no-delimiters (interactive-p)))

(defun ar-doubleslash-date-atpt (&optional no-delimiters)
  "Puts doubled slashes around DATE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'date no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-date-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DATE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'date no-delimiters (interactive-p)))

(defun ar-comment-date-atpt (&optional no-delimiters)
  "Comments DATE at point if any. "
  (interactive "*p")
  (ar-th-comment 'date no-delimiters (interactive-p)))

(defun ar-commatize-date-atpt (&optional no-delimiters)
  "Put a comma after DATE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'date no-delimiters (interactive-p)))

(defun ar-quote-date-atpt (&optional no-delimiters)
  "Put a singlequote before DATE at point if any. "
  (interactive "*p")
  (ar-th-quote 'date no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-date-atpt 'ar-date-hyphen-atpt)
;; ;; (defun ar-date-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around DATE at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'date no-delimiters (interactive-p)))

(defalias 'ar-mark-date-atpt 'ar-date-mark-atpt)
(defun ar-date-mark-atpt ()
  "Marks DATE at point if any. "
  (interactive)
  (ar-th-mark 'date))

(defalias 'ar-hide-date-atpt 'ar-date-hide-atpt)
(defun ar-date-hide-atpt ()
  "Hides DATE at point. "
  (interactive)
  (ar-th-hide 'date))

(defalias 'ar-show-date-atpt 'ar-date-show-atpt)
(defun ar-date-show-atpt ()
  "Shows hidden DATE at point. "
  (interactive)
  (ar-th-show 'date))

(defalias 'ar-hide-show-date-atpt 'ar-date-hide-show-atpt)
(defun ar-date-hide-show-atpt ()
  "Alternatively hides or shows DATE at point. "
  (interactive)
  (ar-th-hide-show 'date))

(defalias 'ar-highlight-date-atpt-mode 'ar-date-highlight-atpt-mode)

(defun ar-date-highlight-atpt-mode (&optional no-delimiters)
  "Toggles date-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'date no-delimiters (interactive-p)))

(defalias 'ar-kill-date-atpt 'ar-date-kill-atpt)
(defun ar-date-kill-atpt (&optional no-delimiters)
  "Kills DATE at point if any. "
  (interactive "*P")
  (ar-th-kill 'date no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-date-atpt 'ar-date-kill-backward-atpt)
(defun ar-date-kill-backward-atpt (&optional no-delimiters)
  "Kills DATE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'date no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-date-atpt 'ar-date-leftrightsinglequote-atpt)
;; ;; (defun ar-date-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'date no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-date-atpt 'ar-date-parentize-atpt)
;; ;; (defun ar-date-parentize-atpt (&optional no-delimiters)
;;   "Parentizes DATE at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'date no-delimiters (interactive-p)))

(defalias 'ar-separate-date-atpt 'ar-date-separate-atpt)
(defun ar-date-separate-atpt (&optional no-delimiters)
  "Separates DATE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'date no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-date-atpt 'ar-date-singlequote-atpt)
;; ;; (defun ar-date-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes DATE at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'date no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-date-atpt 'ar-date-triplequotedq-atpt)
(defun ar-date-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around date. "
  (interactive "*p")
  (ar-th-triplequotedq 'date no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-date-atpt 'ar-date-triplequotesq-atpt)
(defun ar-date-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around date. "
  (interactive "*p")
  (ar-th-triplequotesq 'date no-delimiters (interactive-p)))

(defun ar-underscore-date-atpt (&optional no-delimiters)
  "Put underscore char around DATE. "
  (interactive "*p")
  (ar-th-underscore 'date no-delimiters (interactive-p)))

;; (defalias 'ar-date-whitespace-atpt 'ar-whitespace-date-atpt)
;; ;; (defun ar-whitespace-date-atpt (&optional no-delimiters)
;;   "Put whitespace char around DATE. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'date nil t))

(defalias 'ar-forward-date-atpt 'ar-date-forward-atpt)
(defun ar-date-forward-atpt (&optional arg)
  "Moves forward over DATE at point if any, does nothing otherwise.
Returns end position of DATE "
  (interactive "p")
  (ar-th-forward 'date arg (interactive-p)))

(defalias 'ar-backward-date-atpt 'ar-date-backward-atpt)
(defun ar-date-backward-atpt (&optional arg)
  "Moves backward over DATE before point if any, does nothing otherwise.
Returns beginning position of DATE "
  (interactive "p")
  (ar-th-backward 'date arg (interactive-p)))

(defalias 'ar-transpose-date-atpt 'ar-date-transpose-atpt)
(defun ar-date-transpose-atpt (&optional arg)
  "Transposes DATE with DATE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'date arg (interactive-p)))

(defalias 'ar-sort-date-atpt 'ar-date-sort-atpt)
(defun ar-date-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts dates in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'date reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-date-atpt 'ar-date-check-atpt)
(defun ar-date-check-atpt ()
  "Return t if a DATE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-date-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-date-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-delimited-atpt (&optional arg no-delimiters)
  "Returns delimited at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'delimited arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-delimited-atpt 'ar-delimited-bounds-atpt)
(defun ar-delimited-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of delimited if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'delimited no-delimiters (interactive-p)))

(defun ar-delimited-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DELIMITED at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'delimited no-delimiters (interactive-p)))

(defun ar-delimited-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'delimited no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-delimited-atpt 'ar-delimited-beginning-atpt)
(defun ar-delimited-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'delimited no-delimiters (interactive-p)))

(defalias 'ar-end-of-delimited-atpt 'ar-delimited-end-atpt)
(defun ar-delimited-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'delimited no-delimiters (interactive-p)))

(defalias 'ar-in-delimited-p-atpt 'ar-delimited-in-p-atpt)
(defun ar-delimited-in-p-atpt (&optional no-delimiters)
  "Returns bounds of DELIMITED at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'delimited no-delimiters (interactive-p)))

(defalias 'ar-length-of-delimited-atpt 'ar-delimited-length-atpt)
(defun ar-delimited-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'delimited no-delimiters (interactive-p)))

(defalias 'ar-copy-delimited-atpt 'ar-delimited-copy-atpt)
(defun ar-delimited-copy-atpt (&optional no-delimiters)
  "Returns a copy of DELIMITED at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'delimited no-delimiters (interactive-p)))

(defalias 'ar-delete-delimited-atpt 'ar-delimited-delete-atpt)
(defun ar-delimited-delete-atpt (&optional arg)
  "Deletes DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-delete 'delimited arg (interactive-p)))

(defalias 'ar-delete-delimited-in-region 'ar-delimited-delete-in-region)
(defun ar-delimited-delete-in-region (beg end)
  "Deletes DELIMITED at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'delimited beg end (interactive-p)))

(defun ar-blok-delimited-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around delimited.
  Returns blok or nil if no DELIMITED at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'delimited no-delimiters (interactive-p)))

(defun ar-doublebackslash-delimited-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'delimited no-delimiters (interactive-p)))

(defun ar-doubleslash-delimited-atpt (&optional no-delimiters)
  "Puts doubled slashes around DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'delimited no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-delimited-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'delimited no-delimiters (interactive-p)))

(defun ar-comment-delimited-atpt (&optional no-delimiters)
  "Comments DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-comment 'delimited no-delimiters (interactive-p)))

(defun ar-commatize-delimited-atpt (&optional no-delimiters)
  "Put a comma after DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-commatize 'delimited no-delimiters (interactive-p)))

(defun ar-quote-delimited-atpt (&optional no-delimiters)
  "Put a singlequote before DELIMITED at point if any. "
  (interactive "*p")
  (ar-th-quote 'delimited no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-delimited-atpt 'ar-delimited-hyphen-atpt)
;; ;; (defun ar-delimited-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around DELIMITED at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'delimited no-delimiters (interactive-p)))

(defalias 'ar-mark-delimited-atpt 'ar-delimited-mark-atpt)
(defun ar-delimited-mark-atpt ()
  "Marks DELIMITED at point if any. "
  (interactive)
  (ar-th-mark 'delimited))

(defalias 'ar-hide-delimited-atpt 'ar-delimited-hide-atpt)
(defun ar-delimited-hide-atpt ()
  "Hides DELIMITED at point. "
  (interactive)
  (ar-th-hide 'delimited))

(defalias 'ar-show-delimited-atpt 'ar-delimited-show-atpt)
(defun ar-delimited-show-atpt ()
  "Shows hidden DELIMITED at point. "
  (interactive)
  (ar-th-show 'delimited))

(defalias 'ar-hide-show-delimited-atpt 'ar-delimited-hide-show-atpt)
(defun ar-delimited-hide-show-atpt ()
  "Alternatively hides or shows DELIMITED at point. "
  (interactive)
  (ar-th-hide-show 'delimited))

(defalias 'ar-highlight-delimited-atpt-mode 'ar-delimited-highlight-atpt-mode)

(defun ar-delimited-highlight-atpt-mode (&optional no-delimiters)
  "Toggles delimited-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'delimited no-delimiters (interactive-p)))

(defalias 'ar-kill-delimited-atpt 'ar-delimited-kill-atpt)
(defun ar-delimited-kill-atpt (&optional no-delimiters)
  "Kills DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-kill 'delimited no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-delimited-atpt 'ar-delimited-kill-backward-atpt)
(defun ar-delimited-kill-backward-atpt (&optional no-delimiters)
  "Kills DELIMITED at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'delimited no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-delimited-atpt 'ar-delimited-leftrightsinglequote-atpt)
;; ;; (defun ar-delimited-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'delimited no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-delimited-atpt 'ar-delimited-parentize-atpt)
;; ;; (defun ar-delimited-parentize-atpt (&optional no-delimiters)
;;   "Parentizes DELIMITED at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'delimited no-delimiters (interactive-p)))

(defalias 'ar-separate-delimited-atpt 'ar-delimited-separate-atpt)
(defun ar-delimited-separate-atpt (&optional no-delimiters)
  "Separates DELIMITED at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'delimited no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-delimited-atpt 'ar-delimited-singlequote-atpt)
;; ;; (defun ar-delimited-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes DELIMITED at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'delimited no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-delimited-atpt 'ar-delimited-triplequotedq-atpt)
(defun ar-delimited-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around delimited. "
  (interactive "*p")
  (ar-th-triplequotedq 'delimited no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-delimited-atpt 'ar-delimited-triplequotesq-atpt)
(defun ar-delimited-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around delimited. "
  (interactive "*p")
  (ar-th-triplequotesq 'delimited no-delimiters (interactive-p)))

(defun ar-underscore-delimited-atpt (&optional no-delimiters)
  "Put underscore char around DELIMITED. "
  (interactive "*p")
  (ar-th-underscore 'delimited no-delimiters (interactive-p)))

;; (defalias 'ar-delimited-whitespace-atpt 'ar-whitespace-delimited-atpt)
;; ;; (defun ar-whitespace-delimited-atpt (&optional no-delimiters)
;;   "Put whitespace char around DELIMITED. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'delimited nil t))

(defalias 'ar-forward-delimited-atpt 'ar-delimited-forward-atpt)
(defun ar-delimited-forward-atpt (&optional arg)
  "Moves forward over DELIMITED at point if any, does nothing otherwise.
Returns end position of DELIMITED "
  (interactive "p")
  (ar-th-forward 'delimited arg (interactive-p)))

(defalias 'ar-backward-delimited-atpt 'ar-delimited-backward-atpt)
(defun ar-delimited-backward-atpt (&optional arg)
  "Moves backward over DELIMITED before point if any, does nothing otherwise.
Returns beginning position of DELIMITED "
  (interactive "p")
  (ar-th-backward 'delimited arg (interactive-p)))

(defalias 'ar-transpose-delimited-atpt 'ar-delimited-transpose-atpt)
(defun ar-delimited-transpose-atpt (&optional arg)
  "Transposes DELIMITED with DELIMITED before point if any. "
  (interactive "*p")
  (ar-th-transpose 'delimited arg (interactive-p)))

(defalias 'ar-sort-delimited-atpt 'ar-delimited-sort-atpt)
(defun ar-delimited-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts delimiteds in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'delimited reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-delimited-atpt 'ar-delimited-check-atpt)
(defun ar-delimited-check-atpt ()
  "Return t if a DELIMITED at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-delimited-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-delimited-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-email-atpt (&optional arg no-delimiters)
  "Returns email at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'email arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-email-atpt 'ar-email-bounds-atpt)
(defun ar-email-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of email if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'email no-delimiters (interactive-p)))

(defun ar-email-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position EMAIL at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'email no-delimiters (interactive-p)))

(defun ar-email-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'email no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-email-atpt 'ar-email-beginning-atpt)
(defun ar-email-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'email no-delimiters (interactive-p)))

(defalias 'ar-end-of-email-atpt 'ar-email-end-atpt)
(defun ar-email-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'email no-delimiters (interactive-p)))

(defalias 'ar-in-email-p-atpt 'ar-email-in-p-atpt)
(defun ar-email-in-p-atpt (&optional no-delimiters)
  "Returns bounds of EMAIL at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'email no-delimiters (interactive-p)))

(defalias 'ar-length-of-email-atpt 'ar-email-length-atpt)
(defun ar-email-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'email no-delimiters (interactive-p)))

(defalias 'ar-copy-email-atpt 'ar-email-copy-atpt)
(defun ar-email-copy-atpt (&optional no-delimiters)
  "Returns a copy of EMAIL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'email no-delimiters (interactive-p)))

(defalias 'ar-delete-email-atpt 'ar-email-delete-atpt)
(defun ar-email-delete-atpt (&optional arg)
  "Deletes EMAIL at point if any. "
  (interactive "*p")
  (ar-th-delete 'email arg (interactive-p)))

(defalias 'ar-delete-email-in-region 'ar-email-delete-in-region)
(defun ar-email-delete-in-region (beg end)
  "Deletes EMAIL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'email beg end (interactive-p)))

(defun ar-blok-email-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around email.
  Returns blok or nil if no EMAIL at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'email no-delimiters (interactive-p)))

(defun ar-doublebackslash-email-atpt (&optional no-delimiters)
  "Puts doubled backslashes around EMAIL at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'email no-delimiters (interactive-p)))

(defun ar-doubleslash-email-atpt (&optional no-delimiters)
  "Puts doubled slashes around EMAIL at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'email no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-email-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around EMAIL at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'email no-delimiters (interactive-p)))

(defun ar-comment-email-atpt (&optional no-delimiters)
  "Comments EMAIL at point if any. "
  (interactive "*p")
  (ar-th-comment 'email no-delimiters (interactive-p)))

(defun ar-commatize-email-atpt (&optional no-delimiters)
  "Put a comma after EMAIL at point if any. "
  (interactive "*p")
  (ar-th-commatize 'email no-delimiters (interactive-p)))

(defun ar-quote-email-atpt (&optional no-delimiters)
  "Put a singlequote before EMAIL at point if any. "
  (interactive "*p")
  (ar-th-quote 'email no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-email-atpt 'ar-email-hyphen-atpt)
;; ;; (defun ar-email-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around EMAIL at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'email no-delimiters (interactive-p)))

(defalias 'ar-mark-email-atpt 'ar-email-mark-atpt)
(defun ar-email-mark-atpt ()
  "Marks EMAIL at point if any. "
  (interactive)
  (ar-th-mark 'email))

(defalias 'ar-hide-email-atpt 'ar-email-hide-atpt)
(defun ar-email-hide-atpt ()
  "Hides EMAIL at point. "
  (interactive)
  (ar-th-hide 'email))

(defalias 'ar-show-email-atpt 'ar-email-show-atpt)
(defun ar-email-show-atpt ()
  "Shows hidden EMAIL at point. "
  (interactive)
  (ar-th-show 'email))

(defalias 'ar-hide-show-email-atpt 'ar-email-hide-show-atpt)
(defun ar-email-hide-show-atpt ()
  "Alternatively hides or shows EMAIL at point. "
  (interactive)
  (ar-th-hide-show 'email))

(defalias 'ar-highlight-email-atpt-mode 'ar-email-highlight-atpt-mode)

(defun ar-email-highlight-atpt-mode (&optional no-delimiters)
  "Toggles email-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'email no-delimiters (interactive-p)))

(defalias 'ar-kill-email-atpt 'ar-email-kill-atpt)
(defun ar-email-kill-atpt (&optional no-delimiters)
  "Kills EMAIL at point if any. "
  (interactive "*P")
  (ar-th-kill 'email no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-email-atpt 'ar-email-kill-backward-atpt)
(defun ar-email-kill-backward-atpt (&optional no-delimiters)
  "Kills EMAIL at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'email no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-email-atpt 'ar-email-leftrightsinglequote-atpt)
;; ;; (defun ar-email-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'email no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-email-atpt 'ar-email-parentize-atpt)
;; ;; (defun ar-email-parentize-atpt (&optional no-delimiters)
;;   "Parentizes EMAIL at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'email no-delimiters (interactive-p)))

(defalias 'ar-separate-email-atpt 'ar-email-separate-atpt)
(defun ar-email-separate-atpt (&optional no-delimiters)
  "Separates EMAIL at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'email no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-email-atpt 'ar-email-singlequote-atpt)
;; ;; (defun ar-email-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes EMAIL at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'email no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-email-atpt 'ar-email-triplequotedq-atpt)
(defun ar-email-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around email. "
  (interactive "*p")
  (ar-th-triplequotedq 'email no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-email-atpt 'ar-email-triplequotesq-atpt)
(defun ar-email-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around email. "
  (interactive "*p")
  (ar-th-triplequotesq 'email no-delimiters (interactive-p)))

(defun ar-underscore-email-atpt (&optional no-delimiters)
  "Put underscore char around EMAIL. "
  (interactive "*p")
  (ar-th-underscore 'email no-delimiters (interactive-p)))

;; (defalias 'ar-email-whitespace-atpt 'ar-whitespace-email-atpt)
;; ;; (defun ar-whitespace-email-atpt (&optional no-delimiters)
;;   "Put whitespace char around EMAIL. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'email nil t))

(defalias 'ar-forward-email-atpt 'ar-email-forward-atpt)
(defun ar-email-forward-atpt (&optional arg)
  "Moves forward over EMAIL at point if any, does nothing otherwise.
Returns end position of EMAIL "
  (interactive "p")
  (ar-th-forward 'email arg (interactive-p)))

(defalias 'ar-backward-email-atpt 'ar-email-backward-atpt)
(defun ar-email-backward-atpt (&optional arg)
  "Moves backward over EMAIL before point if any, does nothing otherwise.
Returns beginning position of EMAIL "
  (interactive "p")
  (ar-th-backward 'email arg (interactive-p)))

(defalias 'ar-transpose-email-atpt 'ar-email-transpose-atpt)
(defun ar-email-transpose-atpt (&optional arg)
  "Transposes EMAIL with EMAIL before point if any. "
  (interactive "*p")
  (ar-th-transpose 'email arg (interactive-p)))

(defalias 'ar-sort-email-atpt 'ar-email-sort-atpt)
(defun ar-email-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts emails in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'email reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-email-atpt 'ar-email-check-atpt)
(defun ar-email-check-atpt ()
  "Return t if a EMAIL at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-email-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-email-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-filename-atpt (&optional arg no-delimiters)
  "Returns filename at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'filename arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-filename-atpt 'ar-filename-bounds-atpt)
(defun ar-filename-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of filename if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'filename no-delimiters (interactive-p)))

(defun ar-filename-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position FILENAME at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'filename no-delimiters (interactive-p)))

(defun ar-filename-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'filename no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-filename-atpt 'ar-filename-beginning-atpt)
(defun ar-filename-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'filename no-delimiters (interactive-p)))

(defalias 'ar-end-of-filename-atpt 'ar-filename-end-atpt)
(defun ar-filename-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'filename no-delimiters (interactive-p)))

(defalias 'ar-in-filename-p-atpt 'ar-filename-in-p-atpt)
(defun ar-filename-in-p-atpt (&optional no-delimiters)
  "Returns bounds of FILENAME at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'filename no-delimiters (interactive-p)))

(defalias 'ar-length-of-filename-atpt 'ar-filename-length-atpt)
(defun ar-filename-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'filename no-delimiters (interactive-p)))

(defalias 'ar-copy-filename-atpt 'ar-filename-copy-atpt)
(defun ar-filename-copy-atpt (&optional no-delimiters)
  "Returns a copy of FILENAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'filename no-delimiters (interactive-p)))

(defalias 'ar-delete-filename-atpt 'ar-filename-delete-atpt)
(defun ar-filename-delete-atpt (&optional arg)
  "Deletes FILENAME at point if any. "
  (interactive "*p")
  (ar-th-delete 'filename arg (interactive-p)))

(defalias 'ar-delete-filename-in-region 'ar-filename-delete-in-region)
(defun ar-filename-delete-in-region (beg end)
  "Deletes FILENAME at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'filename beg end (interactive-p)))

(defun ar-blok-filename-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around filename.
  Returns blok or nil if no FILENAME at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'filename no-delimiters (interactive-p)))

(defun ar-doublebackslash-filename-atpt (&optional no-delimiters)
  "Puts doubled backslashes around FILENAME at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'filename no-delimiters (interactive-p)))

(defun ar-doubleslash-filename-atpt (&optional no-delimiters)
  "Puts doubled slashes around FILENAME at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'filename no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-filename-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around FILENAME at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'filename no-delimiters (interactive-p)))

(defun ar-comment-filename-atpt (&optional no-delimiters)
  "Comments FILENAME at point if any. "
  (interactive "*p")
  (ar-th-comment 'filename no-delimiters (interactive-p)))

(defun ar-commatize-filename-atpt (&optional no-delimiters)
  "Put a comma after FILENAME at point if any. "
  (interactive "*p")
  (ar-th-commatize 'filename no-delimiters (interactive-p)))

(defun ar-quote-filename-atpt (&optional no-delimiters)
  "Put a singlequote before FILENAME at point if any. "
  (interactive "*p")
  (ar-th-quote 'filename no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-filename-atpt 'ar-filename-hyphen-atpt)
;; ;; (defun ar-filename-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around FILENAME at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'filename no-delimiters (interactive-p)))

(defalias 'ar-mark-filename-atpt 'ar-filename-mark-atpt)
(defun ar-filename-mark-atpt ()
  "Marks FILENAME at point if any. "
  (interactive)
  (ar-th-mark 'filename))

(defalias 'ar-hide-filename-atpt 'ar-filename-hide-atpt)
(defun ar-filename-hide-atpt ()
  "Hides FILENAME at point. "
  (interactive)
  (ar-th-hide 'filename))

(defalias 'ar-show-filename-atpt 'ar-filename-show-atpt)
(defun ar-filename-show-atpt ()
  "Shows hidden FILENAME at point. "
  (interactive)
  (ar-th-show 'filename))

(defalias 'ar-hide-show-filename-atpt 'ar-filename-hide-show-atpt)
(defun ar-filename-hide-show-atpt ()
  "Alternatively hides or shows FILENAME at point. "
  (interactive)
  (ar-th-hide-show 'filename))

(defalias 'ar-highlight-filename-atpt-mode 'ar-filename-highlight-atpt-mode)

(defun ar-filename-highlight-atpt-mode (&optional no-delimiters)
  "Toggles filename-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'filename no-delimiters (interactive-p)))

(defalias 'ar-kill-filename-atpt 'ar-filename-kill-atpt)
(defun ar-filename-kill-atpt (&optional no-delimiters)
  "Kills FILENAME at point if any. "
  (interactive "*P")
  (ar-th-kill 'filename no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-filename-atpt 'ar-filename-kill-backward-atpt)
(defun ar-filename-kill-backward-atpt (&optional no-delimiters)
  "Kills FILENAME at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'filename no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-filename-atpt 'ar-filename-leftrightsinglequote-atpt)
;; ;; (defun ar-filename-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'filename no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-filename-atpt 'ar-filename-parentize-atpt)
;; ;; (defun ar-filename-parentize-atpt (&optional no-delimiters)
;;   "Parentizes FILENAME at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'filename no-delimiters (interactive-p)))

(defalias 'ar-separate-filename-atpt 'ar-filename-separate-atpt)
(defun ar-filename-separate-atpt (&optional no-delimiters)
  "Separates FILENAME at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'filename no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-filename-atpt 'ar-filename-singlequote-atpt)
;; ;; (defun ar-filename-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes FILENAME at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'filename no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-filename-atpt 'ar-filename-triplequotedq-atpt)
(defun ar-filename-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around filename. "
  (interactive "*p")
  (ar-th-triplequotedq 'filename no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-filename-atpt 'ar-filename-triplequotesq-atpt)
(defun ar-filename-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around filename. "
  (interactive "*p")
  (ar-th-triplequotesq 'filename no-delimiters (interactive-p)))

(defun ar-underscore-filename-atpt (&optional no-delimiters)
  "Put underscore char around FILENAME. "
  (interactive "*p")
  (ar-th-underscore 'filename no-delimiters (interactive-p)))

;; (defalias 'ar-filename-whitespace-atpt 'ar-whitespace-filename-atpt)
;; ;; (defun ar-whitespace-filename-atpt (&optional no-delimiters)
;;   "Put whitespace char around FILENAME. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'filename nil t))

(defalias 'ar-forward-filename-atpt 'ar-filename-forward-atpt)
(defun ar-filename-forward-atpt (&optional arg)
  "Moves forward over FILENAME at point if any, does nothing otherwise.
Returns end position of FILENAME "
  (interactive "p")
  (ar-th-forward 'filename arg (interactive-p)))

(defalias 'ar-backward-filename-atpt 'ar-filename-backward-atpt)
(defun ar-filename-backward-atpt (&optional arg)
  "Moves backward over FILENAME before point if any, does nothing otherwise.
Returns beginning position of FILENAME "
  (interactive "p")
  (ar-th-backward 'filename arg (interactive-p)))

(defalias 'ar-transpose-filename-atpt 'ar-filename-transpose-atpt)
(defun ar-filename-transpose-atpt (&optional arg)
  "Transposes FILENAME with FILENAME before point if any. "
  (interactive "*p")
  (ar-th-transpose 'filename arg (interactive-p)))

(defalias 'ar-sort-filename-atpt 'ar-filename-sort-atpt)
(defun ar-filename-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts filenames in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'filename reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-filename-atpt 'ar-filename-check-atpt)
(defun ar-filename-check-atpt ()
  "Return t if a FILENAME at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-filename-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-filename-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-filenamenondirectory-atpt (&optional arg no-delimiters)
  "Returns filenamenondirectory at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'filenamenondirectory arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-filenamenondirectory-atpt 'ar-filenamenondirectory-bounds-atpt)
(defun ar-filenamenondirectory-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of filenamenondirectory if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'filenamenondirectory no-delimiters (interactive-p)))

(defun ar-filenamenondirectory-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position FILENAMENONDIRECTORY at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'filenamenondirectory no-delimiters (interactive-p)))

(defun ar-filenamenondirectory-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'filenamenondirectory no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-filenamenondirectory-atpt 'ar-filenamenondirectory-beginning-atpt)
(defun ar-filenamenondirectory-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'filenamenondirectory no-delimiters (interactive-p)))

(defalias 'ar-end-of-filenamenondirectory-atpt 'ar-filenamenondirectory-end-atpt)
(defun ar-filenamenondirectory-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'filenamenondirectory no-delimiters (interactive-p)))

(defalias 'ar-in-filenamenondirectory-p-atpt 'ar-filenamenondirectory-in-p-atpt)
(defun ar-filenamenondirectory-in-p-atpt (&optional no-delimiters)
  "Returns bounds of FILENAMENONDIRECTORY at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'filenamenondirectory no-delimiters (interactive-p)))

(defalias 'ar-length-of-filenamenondirectory-atpt 'ar-filenamenondirectory-length-atpt)
(defun ar-filenamenondirectory-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'filenamenondirectory no-delimiters (interactive-p)))

(defalias 'ar-copy-filenamenondirectory-atpt 'ar-filenamenondirectory-copy-atpt)
(defun ar-filenamenondirectory-copy-atpt (&optional no-delimiters)
  "Returns a copy of FILENAMENONDIRECTORY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'filenamenondirectory no-delimiters (interactive-p)))

(defalias 'ar-delete-filenamenondirectory-atpt 'ar-filenamenondirectory-delete-atpt)
(defun ar-filenamenondirectory-delete-atpt (&optional arg)
  "Deletes FILENAMENONDIRECTORY at point if any. "
  (interactive "*p")
  (ar-th-delete 'filenamenondirectory arg (interactive-p)))

(defalias 'ar-delete-filenamenondirectory-in-region 'ar-filenamenondirectory-delete-in-region)
(defun ar-filenamenondirectory-delete-in-region (beg end)
  "Deletes FILENAMENONDIRECTORY at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'filenamenondirectory beg end (interactive-p)))

(defun ar-blok-filenamenondirectory-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around filenamenondirectory.
  Returns blok or nil if no FILENAMENONDIRECTORY at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'filenamenondirectory no-delimiters (interactive-p)))

(defun ar-doublebackslash-filenamenondirectory-atpt (&optional no-delimiters)
  "Puts doubled backslashes around FILENAMENONDIRECTORY at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'filenamenondirectory no-delimiters (interactive-p)))

(defun ar-doubleslash-filenamenondirectory-atpt (&optional no-delimiters)
  "Puts doubled slashes around FILENAMENONDIRECTORY at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'filenamenondirectory no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-filenamenondirectory-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around FILENAMENONDIRECTORY at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'filenamenondirectory no-delimiters (interactive-p)))

(defun ar-comment-filenamenondirectory-atpt (&optional no-delimiters)
  "Comments FILENAMENONDIRECTORY at point if any. "
  (interactive "*p")
  (ar-th-comment 'filenamenondirectory no-delimiters (interactive-p)))

(defun ar-commatize-filenamenondirectory-atpt (&optional no-delimiters)
  "Put a comma after FILENAMENONDIRECTORY at point if any. "
  (interactive "*p")
  (ar-th-commatize 'filenamenondirectory no-delimiters (interactive-p)))

(defun ar-quote-filenamenondirectory-atpt (&optional no-delimiters)
  "Put a singlequote before FILENAMENONDIRECTORY at point if any. "
  (interactive "*p")
  (ar-th-quote 'filenamenondirectory no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-filenamenondirectory-atpt 'ar-filenamenondirectory-hyphen-atpt)
;; ;; (defun ar-filenamenondirectory-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around FILENAMENONDIRECTORY at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'filenamenondirectory no-delimiters (interactive-p)))

(defalias 'ar-mark-filenamenondirectory-atpt 'ar-filenamenondirectory-mark-atpt)
(defun ar-filenamenondirectory-mark-atpt ()
  "Marks FILENAMENONDIRECTORY at point if any. "
  (interactive)
  (ar-th-mark 'filenamenondirectory))

(defalias 'ar-hide-filenamenondirectory-atpt 'ar-filenamenondirectory-hide-atpt)
(defun ar-filenamenondirectory-hide-atpt ()
  "Hides FILENAMENONDIRECTORY at point. "
  (interactive)
  (ar-th-hide 'filenamenondirectory))

(defalias 'ar-show-filenamenondirectory-atpt 'ar-filenamenondirectory-show-atpt)
(defun ar-filenamenondirectory-show-atpt ()
  "Shows hidden FILENAMENONDIRECTORY at point. "
  (interactive)
  (ar-th-show 'filenamenondirectory))

(defalias 'ar-hide-show-filenamenondirectory-atpt 'ar-filenamenondirectory-hide-show-atpt)
(defun ar-filenamenondirectory-hide-show-atpt ()
  "Alternatively hides or shows FILENAMENONDIRECTORY at point. "
  (interactive)
  (ar-th-hide-show 'filenamenondirectory))

(defalias 'ar-highlight-filenamenondirectory-atpt-mode 'ar-filenamenondirectory-highlight-atpt-mode)

(defun ar-filenamenondirectory-highlight-atpt-mode (&optional no-delimiters)
  "Toggles filenamenondirectory-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'filenamenondirectory no-delimiters (interactive-p)))

(defalias 'ar-kill-filenamenondirectory-atpt 'ar-filenamenondirectory-kill-atpt)
(defun ar-filenamenondirectory-kill-atpt (&optional no-delimiters)
  "Kills FILENAMENONDIRECTORY at point if any. "
  (interactive "*P")
  (ar-th-kill 'filenamenondirectory no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-filenamenondirectory-atpt 'ar-filenamenondirectory-kill-backward-atpt)
(defun ar-filenamenondirectory-kill-backward-atpt (&optional no-delimiters)
  "Kills FILENAMENONDIRECTORY at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'filenamenondirectory no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-filenamenondirectory-atpt 'ar-filenamenondirectory-leftrightsinglequote-atpt)
;; ;; (defun ar-filenamenondirectory-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'filenamenondirectory no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-filenamenondirectory-atpt 'ar-filenamenondirectory-parentize-atpt)
;; ;; (defun ar-filenamenondirectory-parentize-atpt (&optional no-delimiters)
;;   "Parentizes FILENAMENONDIRECTORY at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'filenamenondirectory no-delimiters (interactive-p)))

(defalias 'ar-separate-filenamenondirectory-atpt 'ar-filenamenondirectory-separate-atpt)
(defun ar-filenamenondirectory-separate-atpt (&optional no-delimiters)
  "Separates FILENAMENONDIRECTORY at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'filenamenondirectory no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-filenamenondirectory-atpt 'ar-filenamenondirectory-singlequote-atpt)
;; ;; (defun ar-filenamenondirectory-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes FILENAMENONDIRECTORY at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'filenamenondirectory no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-filenamenondirectory-atpt 'ar-filenamenondirectory-triplequotedq-atpt)
(defun ar-filenamenondirectory-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around filenamenondirectory. "
  (interactive "*p")
  (ar-th-triplequotedq 'filenamenondirectory no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-filenamenondirectory-atpt 'ar-filenamenondirectory-triplequotesq-atpt)
(defun ar-filenamenondirectory-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around filenamenondirectory. "
  (interactive "*p")
  (ar-th-triplequotesq 'filenamenondirectory no-delimiters (interactive-p)))

(defun ar-underscore-filenamenondirectory-atpt (&optional no-delimiters)
  "Put underscore char around FILENAMENONDIRECTORY. "
  (interactive "*p")
  (ar-th-underscore 'filenamenondirectory no-delimiters (interactive-p)))

;; (defalias 'ar-filenamenondirectory-whitespace-atpt 'ar-whitespace-filenamenondirectory-atpt)
;; ;; (defun ar-whitespace-filenamenondirectory-atpt (&optional no-delimiters)
;;   "Put whitespace char around FILENAMENONDIRECTORY. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'filenamenondirectory nil t))

(defalias 'ar-forward-filenamenondirectory-atpt 'ar-filenamenondirectory-forward-atpt)
(defun ar-filenamenondirectory-forward-atpt (&optional arg)
  "Moves forward over FILENAMENONDIRECTORY at point if any, does nothing otherwise.
Returns end position of FILENAMENONDIRECTORY "
  (interactive "p")
  (ar-th-forward 'filenamenondirectory arg (interactive-p)))

(defalias 'ar-backward-filenamenondirectory-atpt 'ar-filenamenondirectory-backward-atpt)
(defun ar-filenamenondirectory-backward-atpt (&optional arg)
  "Moves backward over FILENAMENONDIRECTORY before point if any, does nothing otherwise.
Returns beginning position of FILENAMENONDIRECTORY "
  (interactive "p")
  (ar-th-backward 'filenamenondirectory arg (interactive-p)))

(defalias 'ar-transpose-filenamenondirectory-atpt 'ar-filenamenondirectory-transpose-atpt)
(defun ar-filenamenondirectory-transpose-atpt (&optional arg)
  "Transposes FILENAMENONDIRECTORY with FILENAMENONDIRECTORY before point if any. "
  (interactive "*p")
  (ar-th-transpose 'filenamenondirectory arg (interactive-p)))

(defalias 'ar-sort-filenamenondirectory-atpt 'ar-filenamenondirectory-sort-atpt)
(defun ar-filenamenondirectory-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts filenamenondirectorys in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'filenamenondirectory reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-filenamenondirectory-atpt 'ar-filenamenondirectory-check-atpt)
(defun ar-filenamenondirectory-check-atpt ()
  "Return t if a FILENAMENONDIRECTORY at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-filenamenondirectory-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-filenamenondirectory-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-float-atpt (&optional arg no-delimiters)
  "Returns float at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'float arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-float-atpt 'ar-float-bounds-atpt)
(defun ar-float-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of float if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'float no-delimiters (interactive-p)))

(defun ar-float-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position FLOAT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'float no-delimiters (interactive-p)))

(defun ar-float-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'float no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-float-atpt 'ar-float-beginning-atpt)
(defun ar-float-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'float no-delimiters (interactive-p)))

(defalias 'ar-end-of-float-atpt 'ar-float-end-atpt)
(defun ar-float-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'float no-delimiters (interactive-p)))

(defalias 'ar-in-float-p-atpt 'ar-float-in-p-atpt)
(defun ar-float-in-p-atpt (&optional no-delimiters)
  "Returns bounds of FLOAT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'float no-delimiters (interactive-p)))

(defalias 'ar-length-of-float-atpt 'ar-float-length-atpt)
(defun ar-float-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'float no-delimiters (interactive-p)))

(defalias 'ar-copy-float-atpt 'ar-float-copy-atpt)
(defun ar-float-copy-atpt (&optional no-delimiters)
  "Returns a copy of FLOAT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'float no-delimiters (interactive-p)))

(defalias 'ar-delete-float-atpt 'ar-float-delete-atpt)
(defun ar-float-delete-atpt (&optional arg)
  "Deletes FLOAT at point if any. "
  (interactive "*p")
  (ar-th-delete 'float arg (interactive-p)))

(defalias 'ar-delete-float-in-region 'ar-float-delete-in-region)
(defun ar-float-delete-in-region (beg end)
  "Deletes FLOAT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'float beg end (interactive-p)))

(defun ar-blok-float-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around float.
  Returns blok or nil if no FLOAT at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'float no-delimiters (interactive-p)))

(defun ar-doublebackslash-float-atpt (&optional no-delimiters)
  "Puts doubled backslashes around FLOAT at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'float no-delimiters (interactive-p)))

(defun ar-doubleslash-float-atpt (&optional no-delimiters)
  "Puts doubled slashes around FLOAT at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'float no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-float-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around FLOAT at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'float no-delimiters (interactive-p)))

(defun ar-comment-float-atpt (&optional no-delimiters)
  "Comments FLOAT at point if any. "
  (interactive "*p")
  (ar-th-comment 'float no-delimiters (interactive-p)))

(defun ar-commatize-float-atpt (&optional no-delimiters)
  "Put a comma after FLOAT at point if any. "
  (interactive "*p")
  (ar-th-commatize 'float no-delimiters (interactive-p)))

(defun ar-quote-float-atpt (&optional no-delimiters)
  "Put a singlequote before FLOAT at point if any. "
  (interactive "*p")
  (ar-th-quote 'float no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-float-atpt 'ar-float-hyphen-atpt)
;; ;; (defun ar-float-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around FLOAT at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'float no-delimiters (interactive-p)))

(defalias 'ar-mark-float-atpt 'ar-float-mark-atpt)
(defun ar-float-mark-atpt ()
  "Marks FLOAT at point if any. "
  (interactive)
  (ar-th-mark 'float))

(defalias 'ar-hide-float-atpt 'ar-float-hide-atpt)
(defun ar-float-hide-atpt ()
  "Hides FLOAT at point. "
  (interactive)
  (ar-th-hide 'float))

(defalias 'ar-show-float-atpt 'ar-float-show-atpt)
(defun ar-float-show-atpt ()
  "Shows hidden FLOAT at point. "
  (interactive)
  (ar-th-show 'float))

(defalias 'ar-hide-show-float-atpt 'ar-float-hide-show-atpt)
(defun ar-float-hide-show-atpt ()
  "Alternatively hides or shows FLOAT at point. "
  (interactive)
  (ar-th-hide-show 'float))

(defalias 'ar-highlight-float-atpt-mode 'ar-float-highlight-atpt-mode)

(defun ar-float-highlight-atpt-mode (&optional no-delimiters)
  "Toggles float-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'float no-delimiters (interactive-p)))

(defalias 'ar-kill-float-atpt 'ar-float-kill-atpt)
(defun ar-float-kill-atpt (&optional no-delimiters)
  "Kills FLOAT at point if any. "
  (interactive "*P")
  (ar-th-kill 'float no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-float-atpt 'ar-float-kill-backward-atpt)
(defun ar-float-kill-backward-atpt (&optional no-delimiters)
  "Kills FLOAT at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'float no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-float-atpt 'ar-float-leftrightsinglequote-atpt)
;; ;; (defun ar-float-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'float no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-float-atpt 'ar-float-parentize-atpt)
;; ;; (defun ar-float-parentize-atpt (&optional no-delimiters)
;;   "Parentizes FLOAT at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'float no-delimiters (interactive-p)))

(defalias 'ar-separate-float-atpt 'ar-float-separate-atpt)
(defun ar-float-separate-atpt (&optional no-delimiters)
  "Separates FLOAT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'float no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-float-atpt 'ar-float-singlequote-atpt)
;; ;; (defun ar-float-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes FLOAT at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'float no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-float-atpt 'ar-float-triplequotedq-atpt)
(defun ar-float-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around float. "
  (interactive "*p")
  (ar-th-triplequotedq 'float no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-float-atpt 'ar-float-triplequotesq-atpt)
(defun ar-float-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around float. "
  (interactive "*p")
  (ar-th-triplequotesq 'float no-delimiters (interactive-p)))

(defun ar-underscore-float-atpt (&optional no-delimiters)
  "Put underscore char around FLOAT. "
  (interactive "*p")
  (ar-th-underscore 'float no-delimiters (interactive-p)))

;; (defalias 'ar-float-whitespace-atpt 'ar-whitespace-float-atpt)
;; ;; (defun ar-whitespace-float-atpt (&optional no-delimiters)
;;   "Put whitespace char around FLOAT. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'float nil t))

(defalias 'ar-forward-float-atpt 'ar-float-forward-atpt)
(defun ar-float-forward-atpt (&optional arg)
  "Moves forward over FLOAT at point if any, does nothing otherwise.
Returns end position of FLOAT "
  (interactive "p")
  (ar-th-forward 'float arg (interactive-p)))

(defalias 'ar-backward-float-atpt 'ar-float-backward-atpt)
(defun ar-float-backward-atpt (&optional arg)
  "Moves backward over FLOAT before point if any, does nothing otherwise.
Returns beginning position of FLOAT "
  (interactive "p")
  (ar-th-backward 'float arg (interactive-p)))

(defalias 'ar-transpose-float-atpt 'ar-float-transpose-atpt)
(defun ar-float-transpose-atpt (&optional arg)
  "Transposes FLOAT with FLOAT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'float arg (interactive-p)))

(defalias 'ar-sort-float-atpt 'ar-float-sort-atpt)
(defun ar-float-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts floats in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'float reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-float-atpt 'ar-float-check-atpt)
(defun ar-float-check-atpt ()
  "Return t if a FLOAT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-float-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-float-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-function-atpt (&optional arg no-delimiters)
  "Returns function at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'function arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-function-atpt 'ar-function-bounds-atpt)
(defun ar-function-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of function if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'function no-delimiters (interactive-p)))

(defun ar-function-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position FUNCTION at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'function no-delimiters (interactive-p)))

(defun ar-function-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'function no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-function-atpt 'ar-function-beginning-atpt)
(defun ar-function-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'function no-delimiters (interactive-p)))

(defalias 'ar-end-of-function-atpt 'ar-function-end-atpt)
(defun ar-function-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'function no-delimiters (interactive-p)))

(defalias 'ar-in-function-p-atpt 'ar-function-in-p-atpt)
(defun ar-function-in-p-atpt (&optional no-delimiters)
  "Returns bounds of FUNCTION at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'function no-delimiters (interactive-p)))

(defalias 'ar-length-of-function-atpt 'ar-function-length-atpt)
(defun ar-function-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'function no-delimiters (interactive-p)))

(defalias 'ar-copy-function-atpt 'ar-function-copy-atpt)
(defun ar-function-copy-atpt (&optional no-delimiters)
  "Returns a copy of FUNCTION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'function no-delimiters (interactive-p)))

(defalias 'ar-delete-function-atpt 'ar-function-delete-atpt)
(defun ar-function-delete-atpt (&optional arg)
  "Deletes FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-delete 'function arg (interactive-p)))

(defalias 'ar-delete-function-in-region 'ar-function-delete-in-region)
(defun ar-function-delete-in-region (beg end)
  "Deletes FUNCTION at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'function beg end (interactive-p)))

(defun ar-blok-function-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around function.
  Returns blok or nil if no FUNCTION at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'function no-delimiters (interactive-p)))

(defun ar-doublebackslash-function-atpt (&optional no-delimiters)
  "Puts doubled backslashes around FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'function no-delimiters (interactive-p)))

(defun ar-doubleslash-function-atpt (&optional no-delimiters)
  "Puts doubled slashes around FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'function no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-function-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'function no-delimiters (interactive-p)))

(defun ar-comment-function-atpt (&optional no-delimiters)
  "Comments FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-comment 'function no-delimiters (interactive-p)))

(defun ar-commatize-function-atpt (&optional no-delimiters)
  "Put a comma after FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-commatize 'function no-delimiters (interactive-p)))

(defun ar-quote-function-atpt (&optional no-delimiters)
  "Put a singlequote before FUNCTION at point if any. "
  (interactive "*p")
  (ar-th-quote 'function no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-function-atpt 'ar-function-hyphen-atpt)
;; ;; (defun ar-function-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around FUNCTION at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'function no-delimiters (interactive-p)))

(defalias 'ar-mark-function-atpt 'ar-function-mark-atpt)
(defun ar-function-mark-atpt ()
  "Marks FUNCTION at point if any. "
  (interactive)
  (ar-th-mark 'function))

(defalias 'ar-hide-function-atpt 'ar-function-hide-atpt)
(defun ar-function-hide-atpt ()
  "Hides FUNCTION at point. "
  (interactive)
  (ar-th-hide 'function))

(defalias 'ar-show-function-atpt 'ar-function-show-atpt)
(defun ar-function-show-atpt ()
  "Shows hidden FUNCTION at point. "
  (interactive)
  (ar-th-show 'function))

(defalias 'ar-hide-show-function-atpt 'ar-function-hide-show-atpt)
(defun ar-function-hide-show-atpt ()
  "Alternatively hides or shows FUNCTION at point. "
  (interactive)
  (ar-th-hide-show 'function))

(defalias 'ar-highlight-function-atpt-mode 'ar-function-highlight-atpt-mode)

(defun ar-function-highlight-atpt-mode (&optional no-delimiters)
  "Toggles function-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'function no-delimiters (interactive-p)))

(defalias 'ar-kill-function-atpt 'ar-function-kill-atpt)
(defun ar-function-kill-atpt (&optional no-delimiters)
  "Kills FUNCTION at point if any. "
  (interactive "*P")
  (ar-th-kill 'function no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-function-atpt 'ar-function-kill-backward-atpt)
(defun ar-function-kill-backward-atpt (&optional no-delimiters)
  "Kills FUNCTION at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'function no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-function-atpt 'ar-function-leftrightsinglequote-atpt)
;; ;; (defun ar-function-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'function no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-function-atpt 'ar-function-parentize-atpt)
;; ;; (defun ar-function-parentize-atpt (&optional no-delimiters)
;;   "Parentizes FUNCTION at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'function no-delimiters (interactive-p)))

(defalias 'ar-separate-function-atpt 'ar-function-separate-atpt)
(defun ar-function-separate-atpt (&optional no-delimiters)
  "Separates FUNCTION at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'function no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-function-atpt 'ar-function-singlequote-atpt)
;; ;; (defun ar-function-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes FUNCTION at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'function no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-function-atpt 'ar-function-triplequotedq-atpt)
(defun ar-function-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around function. "
  (interactive "*p")
  (ar-th-triplequotedq 'function no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-function-atpt 'ar-function-triplequotesq-atpt)
(defun ar-function-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around function. "
  (interactive "*p")
  (ar-th-triplequotesq 'function no-delimiters (interactive-p)))

(defun ar-underscore-function-atpt (&optional no-delimiters)
  "Put underscore char around FUNCTION. "
  (interactive "*p")
  (ar-th-underscore 'function no-delimiters (interactive-p)))

;; (defalias 'ar-function-whitespace-atpt 'ar-whitespace-function-atpt)
;; ;; (defun ar-whitespace-function-atpt (&optional no-delimiters)
;;   "Put whitespace char around FUNCTION. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'function nil t))

(defalias 'ar-forward-function-atpt 'ar-function-forward-atpt)
(defun ar-function-forward-atpt (&optional arg)
  "Moves forward over FUNCTION at point if any, does nothing otherwise.
Returns end position of FUNCTION "
  (interactive "p")
  (ar-th-forward 'function arg (interactive-p)))

(defalias 'ar-backward-function-atpt 'ar-function-backward-atpt)
(defun ar-function-backward-atpt (&optional arg)
  "Moves backward over FUNCTION before point if any, does nothing otherwise.
Returns beginning position of FUNCTION "
  (interactive "p")
  (ar-th-backward 'function arg (interactive-p)))

(defalias 'ar-transpose-function-atpt 'ar-function-transpose-atpt)
(defun ar-function-transpose-atpt (&optional arg)
  "Transposes FUNCTION with FUNCTION before point if any. "
  (interactive "*p")
  (ar-th-transpose 'function arg (interactive-p)))

(defalias 'ar-sort-function-atpt 'ar-function-sort-atpt)
(defun ar-function-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts functions in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'function reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-function-atpt 'ar-function-check-atpt)
(defun ar-function-check-atpt ()
  "Return t if a FUNCTION at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-function-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-function-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-ip-atpt (&optional arg no-delimiters)
  "Returns ip at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'ip arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-ip-atpt 'ar-ip-bounds-atpt)
(defun ar-ip-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of ip if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'ip no-delimiters (interactive-p)))

(defun ar-ip-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position IP at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'ip no-delimiters (interactive-p)))

(defun ar-ip-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'ip no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-ip-atpt 'ar-ip-beginning-atpt)
(defun ar-ip-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'ip no-delimiters (interactive-p)))

(defalias 'ar-end-of-ip-atpt 'ar-ip-end-atpt)
(defun ar-ip-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'ip no-delimiters (interactive-p)))

(defalias 'ar-in-ip-p-atpt 'ar-ip-in-p-atpt)
(defun ar-ip-in-p-atpt (&optional no-delimiters)
  "Returns bounds of IP at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'ip no-delimiters (interactive-p)))

(defalias 'ar-length-of-ip-atpt 'ar-ip-length-atpt)
(defun ar-ip-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'ip no-delimiters (interactive-p)))

(defalias 'ar-copy-ip-atpt 'ar-ip-copy-atpt)
(defun ar-ip-copy-atpt (&optional no-delimiters)
  "Returns a copy of IP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'ip no-delimiters (interactive-p)))

(defalias 'ar-delete-ip-atpt 'ar-ip-delete-atpt)
(defun ar-ip-delete-atpt (&optional arg)
  "Deletes IP at point if any. "
  (interactive "*p")
  (ar-th-delete 'ip arg (interactive-p)))

(defalias 'ar-delete-ip-in-region 'ar-ip-delete-in-region)
(defun ar-ip-delete-in-region (beg end)
  "Deletes IP at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'ip beg end (interactive-p)))

(defun ar-blok-ip-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around ip.
  Returns blok or nil if no IP at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'ip no-delimiters (interactive-p)))

(defun ar-doublebackslash-ip-atpt (&optional no-delimiters)
  "Puts doubled backslashes around IP at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'ip no-delimiters (interactive-p)))

(defun ar-doubleslash-ip-atpt (&optional no-delimiters)
  "Puts doubled slashes around IP at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'ip no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-ip-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around IP at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'ip no-delimiters (interactive-p)))

(defun ar-comment-ip-atpt (&optional no-delimiters)
  "Comments IP at point if any. "
  (interactive "*p")
  (ar-th-comment 'ip no-delimiters (interactive-p)))

(defun ar-commatize-ip-atpt (&optional no-delimiters)
  "Put a comma after IP at point if any. "
  (interactive "*p")
  (ar-th-commatize 'ip no-delimiters (interactive-p)))

(defun ar-quote-ip-atpt (&optional no-delimiters)
  "Put a singlequote before IP at point if any. "
  (interactive "*p")
  (ar-th-quote 'ip no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-ip-atpt 'ar-ip-hyphen-atpt)
;; ;; (defun ar-ip-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around IP at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'ip no-delimiters (interactive-p)))

(defalias 'ar-mark-ip-atpt 'ar-ip-mark-atpt)
(defun ar-ip-mark-atpt ()
  "Marks IP at point if any. "
  (interactive)
  (ar-th-mark 'ip))

(defalias 'ar-hide-ip-atpt 'ar-ip-hide-atpt)
(defun ar-ip-hide-atpt ()
  "Hides IP at point. "
  (interactive)
  (ar-th-hide 'ip))

(defalias 'ar-show-ip-atpt 'ar-ip-show-atpt)
(defun ar-ip-show-atpt ()
  "Shows hidden IP at point. "
  (interactive)
  (ar-th-show 'ip))

(defalias 'ar-hide-show-ip-atpt 'ar-ip-hide-show-atpt)
(defun ar-ip-hide-show-atpt ()
  "Alternatively hides or shows IP at point. "
  (interactive)
  (ar-th-hide-show 'ip))

(defalias 'ar-highlight-ip-atpt-mode 'ar-ip-highlight-atpt-mode)

(defun ar-ip-highlight-atpt-mode (&optional no-delimiters)
  "Toggles ip-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'ip no-delimiters (interactive-p)))

(defalias 'ar-kill-ip-atpt 'ar-ip-kill-atpt)
(defun ar-ip-kill-atpt (&optional no-delimiters)
  "Kills IP at point if any. "
  (interactive "*P")
  (ar-th-kill 'ip no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-ip-atpt 'ar-ip-kill-backward-atpt)
(defun ar-ip-kill-backward-atpt (&optional no-delimiters)
  "Kills IP at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'ip no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-ip-atpt 'ar-ip-leftrightsinglequote-atpt)
;; ;; (defun ar-ip-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'ip no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-ip-atpt 'ar-ip-parentize-atpt)
;; ;; (defun ar-ip-parentize-atpt (&optional no-delimiters)
;;   "Parentizes IP at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'ip no-delimiters (interactive-p)))

(defalias 'ar-separate-ip-atpt 'ar-ip-separate-atpt)
(defun ar-ip-separate-atpt (&optional no-delimiters)
  "Separates IP at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'ip no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-ip-atpt 'ar-ip-singlequote-atpt)
;; ;; (defun ar-ip-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes IP at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'ip no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-ip-atpt 'ar-ip-triplequotedq-atpt)
(defun ar-ip-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around ip. "
  (interactive "*p")
  (ar-th-triplequotedq 'ip no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-ip-atpt 'ar-ip-triplequotesq-atpt)
(defun ar-ip-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around ip. "
  (interactive "*p")
  (ar-th-triplequotesq 'ip no-delimiters (interactive-p)))

(defun ar-underscore-ip-atpt (&optional no-delimiters)
  "Put underscore char around IP. "
  (interactive "*p")
  (ar-th-underscore 'ip no-delimiters (interactive-p)))

;; (defalias 'ar-ip-whitespace-atpt 'ar-whitespace-ip-atpt)
;; ;; (defun ar-whitespace-ip-atpt (&optional no-delimiters)
;;   "Put whitespace char around IP. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'ip nil t))

(defalias 'ar-forward-ip-atpt 'ar-ip-forward-atpt)
(defun ar-ip-forward-atpt (&optional arg)
  "Moves forward over IP at point if any, does nothing otherwise.
Returns end position of IP "
  (interactive "p")
  (ar-th-forward 'ip arg (interactive-p)))

(defalias 'ar-backward-ip-atpt 'ar-ip-backward-atpt)
(defun ar-ip-backward-atpt (&optional arg)
  "Moves backward over IP before point if any, does nothing otherwise.
Returns beginning position of IP "
  (interactive "p")
  (ar-th-backward 'ip arg (interactive-p)))

(defalias 'ar-transpose-ip-atpt 'ar-ip-transpose-atpt)
(defun ar-ip-transpose-atpt (&optional arg)
  "Transposes IP with IP before point if any. "
  (interactive "*p")
  (ar-th-transpose 'ip arg (interactive-p)))

(defalias 'ar-sort-ip-atpt 'ar-ip-sort-atpt)
(defun ar-ip-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts ips in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'ip reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-ip-atpt 'ar-ip-check-atpt)
(defun ar-ip-check-atpt ()
  "Return t if a IP at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-ip-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-ip-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-isbn-atpt (&optional arg no-delimiters)
  "Returns isbn at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'isbn arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-isbn-atpt 'ar-isbn-bounds-atpt)
(defun ar-isbn-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of isbn if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'isbn no-delimiters (interactive-p)))

(defun ar-isbn-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position ISBN at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'isbn no-delimiters (interactive-p)))

(defun ar-isbn-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'isbn no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-isbn-atpt 'ar-isbn-beginning-atpt)
(defun ar-isbn-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'isbn no-delimiters (interactive-p)))

(defalias 'ar-end-of-isbn-atpt 'ar-isbn-end-atpt)
(defun ar-isbn-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'isbn no-delimiters (interactive-p)))

(defalias 'ar-in-isbn-p-atpt 'ar-isbn-in-p-atpt)
(defun ar-isbn-in-p-atpt (&optional no-delimiters)
  "Returns bounds of ISBN at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'isbn no-delimiters (interactive-p)))

(defalias 'ar-length-of-isbn-atpt 'ar-isbn-length-atpt)
(defun ar-isbn-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'isbn no-delimiters (interactive-p)))

(defalias 'ar-copy-isbn-atpt 'ar-isbn-copy-atpt)
(defun ar-isbn-copy-atpt (&optional no-delimiters)
  "Returns a copy of ISBN at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'isbn no-delimiters (interactive-p)))

(defalias 'ar-delete-isbn-atpt 'ar-isbn-delete-atpt)
(defun ar-isbn-delete-atpt (&optional arg)
  "Deletes ISBN at point if any. "
  (interactive "*p")
  (ar-th-delete 'isbn arg (interactive-p)))

(defalias 'ar-delete-isbn-in-region 'ar-isbn-delete-in-region)
(defun ar-isbn-delete-in-region (beg end)
  "Deletes ISBN at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'isbn beg end (interactive-p)))

(defun ar-blok-isbn-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around isbn.
  Returns blok or nil if no ISBN at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'isbn no-delimiters (interactive-p)))

(defun ar-doublebackslash-isbn-atpt (&optional no-delimiters)
  "Puts doubled backslashes around ISBN at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'isbn no-delimiters (interactive-p)))

(defun ar-doubleslash-isbn-atpt (&optional no-delimiters)
  "Puts doubled slashes around ISBN at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'isbn no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-isbn-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around ISBN at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'isbn no-delimiters (interactive-p)))

(defun ar-comment-isbn-atpt (&optional no-delimiters)
  "Comments ISBN at point if any. "
  (interactive "*p")
  (ar-th-comment 'isbn no-delimiters (interactive-p)))

(defun ar-commatize-isbn-atpt (&optional no-delimiters)
  "Put a comma after ISBN at point if any. "
  (interactive "*p")
  (ar-th-commatize 'isbn no-delimiters (interactive-p)))

(defun ar-quote-isbn-atpt (&optional no-delimiters)
  "Put a singlequote before ISBN at point if any. "
  (interactive "*p")
  (ar-th-quote 'isbn no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-isbn-atpt 'ar-isbn-hyphen-atpt)
;; ;; (defun ar-isbn-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around ISBN at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'isbn no-delimiters (interactive-p)))

(defalias 'ar-mark-isbn-atpt 'ar-isbn-mark-atpt)
(defun ar-isbn-mark-atpt ()
  "Marks ISBN at point if any. "
  (interactive)
  (ar-th-mark 'isbn))

(defalias 'ar-hide-isbn-atpt 'ar-isbn-hide-atpt)
(defun ar-isbn-hide-atpt ()
  "Hides ISBN at point. "
  (interactive)
  (ar-th-hide 'isbn))

(defalias 'ar-show-isbn-atpt 'ar-isbn-show-atpt)
(defun ar-isbn-show-atpt ()
  "Shows hidden ISBN at point. "
  (interactive)
  (ar-th-show 'isbn))

(defalias 'ar-hide-show-isbn-atpt 'ar-isbn-hide-show-atpt)
(defun ar-isbn-hide-show-atpt ()
  "Alternatively hides or shows ISBN at point. "
  (interactive)
  (ar-th-hide-show 'isbn))

(defalias 'ar-highlight-isbn-atpt-mode 'ar-isbn-highlight-atpt-mode)

(defun ar-isbn-highlight-atpt-mode (&optional no-delimiters)
  "Toggles isbn-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'isbn no-delimiters (interactive-p)))

(defalias 'ar-kill-isbn-atpt 'ar-isbn-kill-atpt)
(defun ar-isbn-kill-atpt (&optional no-delimiters)
  "Kills ISBN at point if any. "
  (interactive "*P")
  (ar-th-kill 'isbn no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-isbn-atpt 'ar-isbn-kill-backward-atpt)
(defun ar-isbn-kill-backward-atpt (&optional no-delimiters)
  "Kills ISBN at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'isbn no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-isbn-atpt 'ar-isbn-leftrightsinglequote-atpt)
;; ;; (defun ar-isbn-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'isbn no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-isbn-atpt 'ar-isbn-parentize-atpt)
;; ;; (defun ar-isbn-parentize-atpt (&optional no-delimiters)
;;   "Parentizes ISBN at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'isbn no-delimiters (interactive-p)))

(defalias 'ar-separate-isbn-atpt 'ar-isbn-separate-atpt)
(defun ar-isbn-separate-atpt (&optional no-delimiters)
  "Separates ISBN at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'isbn no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-isbn-atpt 'ar-isbn-singlequote-atpt)
;; ;; (defun ar-isbn-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes ISBN at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'isbn no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-isbn-atpt 'ar-isbn-triplequotedq-atpt)
(defun ar-isbn-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around isbn. "
  (interactive "*p")
  (ar-th-triplequotedq 'isbn no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-isbn-atpt 'ar-isbn-triplequotesq-atpt)
(defun ar-isbn-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around isbn. "
  (interactive "*p")
  (ar-th-triplequotesq 'isbn no-delimiters (interactive-p)))

(defun ar-underscore-isbn-atpt (&optional no-delimiters)
  "Put underscore char around ISBN. "
  (interactive "*p")
  (ar-th-underscore 'isbn no-delimiters (interactive-p)))

;; (defalias 'ar-isbn-whitespace-atpt 'ar-whitespace-isbn-atpt)
;; ;; (defun ar-whitespace-isbn-atpt (&optional no-delimiters)
;;   "Put whitespace char around ISBN. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'isbn nil t))

(defalias 'ar-forward-isbn-atpt 'ar-isbn-forward-atpt)
(defun ar-isbn-forward-atpt (&optional arg)
  "Moves forward over ISBN at point if any, does nothing otherwise.
Returns end position of ISBN "
  (interactive "p")
  (ar-th-forward 'isbn arg (interactive-p)))

(defalias 'ar-backward-isbn-atpt 'ar-isbn-backward-atpt)
(defun ar-isbn-backward-atpt (&optional arg)
  "Moves backward over ISBN before point if any, does nothing otherwise.
Returns beginning position of ISBN "
  (interactive "p")
  (ar-th-backward 'isbn arg (interactive-p)))

(defalias 'ar-transpose-isbn-atpt 'ar-isbn-transpose-atpt)
(defun ar-isbn-transpose-atpt (&optional arg)
  "Transposes ISBN with ISBN before point if any. "
  (interactive "*p")
  (ar-th-transpose 'isbn arg (interactive-p)))

(defalias 'ar-sort-isbn-atpt 'ar-isbn-sort-atpt)
(defun ar-isbn-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts isbns in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'isbn reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-isbn-atpt 'ar-isbn-check-atpt)
(defun ar-isbn-check-atpt ()
  "Return t if a ISBN at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-isbn-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-isbn-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-line-atpt (&optional arg no-delimiters)
  "Returns line at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'line arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-line-atpt 'ar-line-bounds-atpt)
(defun ar-line-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of line if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'line no-delimiters (interactive-p)))

(defun ar-line-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position LINE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'line no-delimiters (interactive-p)))

(defun ar-line-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'line no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-line-atpt 'ar-line-beginning-atpt)
(defun ar-line-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'line no-delimiters (interactive-p)))

(defalias 'ar-end-of-line-atpt 'ar-line-end-atpt)
(defun ar-line-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'line no-delimiters (interactive-p)))

(defalias 'ar-in-line-p-atpt 'ar-line-in-p-atpt)
(defun ar-line-in-p-atpt (&optional no-delimiters)
  "Returns bounds of LINE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'line no-delimiters (interactive-p)))

(defalias 'ar-length-of-line-atpt 'ar-line-length-atpt)
(defun ar-line-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'line no-delimiters (interactive-p)))

(defalias 'ar-copy-line-atpt 'ar-line-copy-atpt)
(defun ar-line-copy-atpt (&optional no-delimiters)
  "Returns a copy of LINE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'line no-delimiters (interactive-p)))

(defalias 'ar-delete-line-atpt 'ar-line-delete-atpt)
(defun ar-line-delete-atpt (&optional arg)
  "Deletes LINE at point if any. "
  (interactive "*p")
  (ar-th-delete 'line arg (interactive-p)))

(defalias 'ar-delete-line-in-region 'ar-line-delete-in-region)
(defun ar-line-delete-in-region (beg end)
  "Deletes LINE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'line beg end (interactive-p)))

(defun ar-blok-line-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around line.
  Returns blok or nil if no LINE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'line no-delimiters (interactive-p)))

(defun ar-doublebackslash-line-atpt (&optional no-delimiters)
  "Puts doubled backslashes around LINE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'line no-delimiters (interactive-p)))

(defun ar-doubleslash-line-atpt (&optional no-delimiters)
  "Puts doubled slashes around LINE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'line no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-line-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around LINE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'line no-delimiters (interactive-p)))

(defun ar-comment-line-atpt (&optional no-delimiters)
  "Comments LINE at point if any. "
  (interactive "*p")
  (ar-th-comment 'line no-delimiters (interactive-p)))

(defun ar-commatize-line-atpt (&optional no-delimiters)
  "Put a comma after LINE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'line no-delimiters (interactive-p)))

(defun ar-quote-line-atpt (&optional no-delimiters)
  "Put a singlequote before LINE at point if any. "
  (interactive "*p")
  (ar-th-quote 'line no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-line-atpt 'ar-line-hyphen-atpt)
;; ;; (defun ar-line-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around LINE at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'line no-delimiters (interactive-p)))

(defalias 'ar-mark-line-atpt 'ar-line-mark-atpt)
(defun ar-line-mark-atpt ()
  "Marks LINE at point if any. "
  (interactive)
  (ar-th-mark 'line))

(defalias 'ar-hide-line-atpt 'ar-line-hide-atpt)
(defun ar-line-hide-atpt ()
  "Hides LINE at point. "
  (interactive)
  (ar-th-hide 'line))

(defalias 'ar-show-line-atpt 'ar-line-show-atpt)
(defun ar-line-show-atpt ()
  "Shows hidden LINE at point. "
  (interactive)
  (ar-th-show 'line))

(defalias 'ar-hide-show-line-atpt 'ar-line-hide-show-atpt)
(defun ar-line-hide-show-atpt ()
  "Alternatively hides or shows LINE at point. "
  (interactive)
  (ar-th-hide-show 'line))

(defalias 'ar-highlight-line-atpt-mode 'ar-line-highlight-atpt-mode)

(defun ar-line-highlight-atpt-mode (&optional no-delimiters)
  "Toggles line-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'line no-delimiters (interactive-p)))

(defalias 'ar-kill-line-atpt 'ar-line-kill-atpt)
(defun ar-line-kill-atpt (&optional no-delimiters)
  "Kills LINE at point if any. "
  (interactive "*P")
  (ar-th-kill 'line no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-line-atpt 'ar-line-kill-backward-atpt)
(defun ar-line-kill-backward-atpt (&optional no-delimiters)
  "Kills LINE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'line no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-line-atpt 'ar-line-leftrightsinglequote-atpt)
;; ;; (defun ar-line-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'line no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-line-atpt 'ar-line-parentize-atpt)
;; ;; (defun ar-line-parentize-atpt (&optional no-delimiters)
;;   "Parentizes LINE at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'line no-delimiters (interactive-p)))

(defalias 'ar-separate-line-atpt 'ar-line-separate-atpt)
(defun ar-line-separate-atpt (&optional no-delimiters)
  "Separates LINE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'line no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-line-atpt 'ar-line-singlequote-atpt)
;; ;; (defun ar-line-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes LINE at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'line no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-line-atpt 'ar-line-triplequotedq-atpt)
(defun ar-line-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around line. "
  (interactive "*p")
  (ar-th-triplequotedq 'line no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-line-atpt 'ar-line-triplequotesq-atpt)
(defun ar-line-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around line. "
  (interactive "*p")
  (ar-th-triplequotesq 'line no-delimiters (interactive-p)))

(defun ar-underscore-line-atpt (&optional no-delimiters)
  "Put underscore char around LINE. "
  (interactive "*p")
  (ar-th-underscore 'line no-delimiters (interactive-p)))

;; (defalias 'ar-line-whitespace-atpt 'ar-whitespace-line-atpt)
;; ;; (defun ar-whitespace-line-atpt (&optional no-delimiters)
;;   "Put whitespace char around LINE. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'line nil t))

(defalias 'ar-forward-line-atpt 'ar-line-forward-atpt)
(defun ar-line-forward-atpt (&optional arg)
  "Moves forward over LINE at point if any, does nothing otherwise.
Returns end position of LINE "
  (interactive "p")
  (ar-th-forward 'line arg (interactive-p)))

(defalias 'ar-backward-line-atpt 'ar-line-backward-atpt)
(defun ar-line-backward-atpt (&optional arg)
  "Moves backward over LINE before point if any, does nothing otherwise.
Returns beginning position of LINE "
  (interactive "p")
  (ar-th-backward 'line arg (interactive-p)))

(defalias 'ar-transpose-line-atpt 'ar-line-transpose-atpt)
(defun ar-line-transpose-atpt (&optional arg)
  "Transposes LINE with LINE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'line arg (interactive-p)))

(defalias 'ar-sort-line-atpt 'ar-line-sort-atpt)
(defun ar-line-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lines in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'line reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-line-atpt 'ar-line-check-atpt)
(defun ar-line-check-atpt ()
  "Return t if a LINE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-line-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-line-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-list-atpt (&optional arg no-delimiters)
  "Returns list at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'list arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-list-atpt 'ar-list-bounds-atpt)
(defun ar-list-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of list if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'list no-delimiters (interactive-p)))

(defun ar-list-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position LIST at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'list no-delimiters (interactive-p)))

(defun ar-list-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'list no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-list-atpt 'ar-list-beginning-atpt)
(defun ar-list-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'list no-delimiters (interactive-p)))

(defalias 'ar-end-of-list-atpt 'ar-list-end-atpt)
(defun ar-list-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'list no-delimiters (interactive-p)))

(defalias 'ar-in-list-p-atpt 'ar-list-in-p-atpt)
(defun ar-list-in-p-atpt (&optional no-delimiters)
  "Returns bounds of LIST at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'list no-delimiters (interactive-p)))

(defalias 'ar-length-of-list-atpt 'ar-list-length-atpt)
(defun ar-list-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'list no-delimiters (interactive-p)))

(defalias 'ar-copy-list-atpt 'ar-list-copy-atpt)
(defun ar-list-copy-atpt (&optional no-delimiters)
  "Returns a copy of LIST at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'list no-delimiters (interactive-p)))

(defalias 'ar-delete-list-atpt 'ar-list-delete-atpt)
(defun ar-list-delete-atpt (&optional arg)
  "Deletes LIST at point if any. "
  (interactive "*p")
  (ar-th-delete 'list arg (interactive-p)))

(defalias 'ar-delete-list-in-region 'ar-list-delete-in-region)
(defun ar-list-delete-in-region (beg end)
  "Deletes LIST at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'list beg end (interactive-p)))

(defun ar-blok-list-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around list.
  Returns blok or nil if no LIST at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'list no-delimiters (interactive-p)))

(defun ar-doublebackslash-list-atpt (&optional no-delimiters)
  "Puts doubled backslashes around LIST at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'list no-delimiters (interactive-p)))

(defun ar-doubleslash-list-atpt (&optional no-delimiters)
  "Puts doubled slashes around LIST at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'list no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-list-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around LIST at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'list no-delimiters (interactive-p)))

(defun ar-comment-list-atpt (&optional no-delimiters)
  "Comments LIST at point if any. "
  (interactive "*p")
  (ar-th-comment 'list no-delimiters (interactive-p)))

(defun ar-commatize-list-atpt (&optional no-delimiters)
  "Put a comma after LIST at point if any. "
  (interactive "*p")
  (ar-th-commatize 'list no-delimiters (interactive-p)))

(defun ar-quote-list-atpt (&optional no-delimiters)
  "Put a singlequote before LIST at point if any. "
  (interactive "*p")
  (ar-th-quote 'list no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-list-atpt 'ar-list-hyphen-atpt)
;; ;; (defun ar-list-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around LIST at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'list no-delimiters (interactive-p)))

(defalias 'ar-mark-list-atpt 'ar-list-mark-atpt)
(defun ar-list-mark-atpt ()
  "Marks LIST at point if any. "
  (interactive)
  (ar-th-mark 'list))

(defalias 'ar-hide-list-atpt 'ar-list-hide-atpt)
(defun ar-list-hide-atpt ()
  "Hides LIST at point. "
  (interactive)
  (ar-th-hide 'list))

(defalias 'ar-show-list-atpt 'ar-list-show-atpt)
(defun ar-list-show-atpt ()
  "Shows hidden LIST at point. "
  (interactive)
  (ar-th-show 'list))

(defalias 'ar-hide-show-list-atpt 'ar-list-hide-show-atpt)
(defun ar-list-hide-show-atpt ()
  "Alternatively hides or shows LIST at point. "
  (interactive)
  (ar-th-hide-show 'list))

(defalias 'ar-highlight-list-atpt-mode 'ar-list-highlight-atpt-mode)

(defun ar-list-highlight-atpt-mode (&optional no-delimiters)
  "Toggles list-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'list no-delimiters (interactive-p)))

(defalias 'ar-kill-list-atpt 'ar-list-kill-atpt)
(defun ar-list-kill-atpt (&optional no-delimiters)
  "Kills LIST at point if any. "
  (interactive "*P")
  (ar-th-kill 'list no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-list-atpt 'ar-list-kill-backward-atpt)
(defun ar-list-kill-backward-atpt (&optional no-delimiters)
  "Kills LIST at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'list no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-list-atpt 'ar-list-leftrightsinglequote-atpt)
;; ;; (defun ar-list-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'list no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-list-atpt 'ar-list-parentize-atpt)
;; ;; (defun ar-list-parentize-atpt (&optional no-delimiters)
;;   "Parentizes LIST at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'list no-delimiters (interactive-p)))

(defalias 'ar-separate-list-atpt 'ar-list-separate-atpt)
(defun ar-list-separate-atpt (&optional no-delimiters)
  "Separates LIST at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'list no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-list-atpt 'ar-list-singlequote-atpt)
;; ;; (defun ar-list-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes LIST at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'list no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-list-atpt 'ar-list-triplequotedq-atpt)
(defun ar-list-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around list. "
  (interactive "*p")
  (ar-th-triplequotedq 'list no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-list-atpt 'ar-list-triplequotesq-atpt)
(defun ar-list-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around list. "
  (interactive "*p")
  (ar-th-triplequotesq 'list no-delimiters (interactive-p)))

(defun ar-underscore-list-atpt (&optional no-delimiters)
  "Put underscore char around LIST. "
  (interactive "*p")
  (ar-th-underscore 'list no-delimiters (interactive-p)))

;; (defalias 'ar-list-whitespace-atpt 'ar-whitespace-list-atpt)
;; ;; (defun ar-whitespace-list-atpt (&optional no-delimiters)
;;   "Put whitespace char around LIST. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'list nil t))

(defalias 'ar-forward-list-atpt 'ar-list-forward-atpt)
(defun ar-list-forward-atpt (&optional arg)
  "Moves forward over LIST at point if any, does nothing otherwise.
Returns end position of LIST "
  (interactive "p")
  (ar-th-forward 'list arg (interactive-p)))

(defalias 'ar-backward-list-atpt 'ar-list-backward-atpt)
(defun ar-list-backward-atpt (&optional arg)
  "Moves backward over LIST before point if any, does nothing otherwise.
Returns beginning position of LIST "
  (interactive "p")
  (ar-th-backward 'list arg (interactive-p)))

(defalias 'ar-transpose-list-atpt 'ar-list-transpose-atpt)
(defun ar-list-transpose-atpt (&optional arg)
  "Transposes LIST with LIST before point if any. "
  (interactive "*p")
  (ar-th-transpose 'list arg (interactive-p)))

(defalias 'ar-sort-list-atpt 'ar-list-sort-atpt)
(defun ar-list-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts lists in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'list reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-list-atpt 'ar-list-check-atpt)
(defun ar-list-check-atpt ()
  "Return t if a LIST at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-list-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-list-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-name-atpt (&optional arg no-delimiters)
  "Returns name at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'name arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-name-atpt 'ar-name-bounds-atpt)
(defun ar-name-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of name if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'name no-delimiters (interactive-p)))

(defun ar-name-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position NAME at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'name no-delimiters (interactive-p)))

(defun ar-name-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'name no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-name-atpt 'ar-name-beginning-atpt)
(defun ar-name-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'name no-delimiters (interactive-p)))

(defalias 'ar-end-of-name-atpt 'ar-name-end-atpt)
(defun ar-name-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'name no-delimiters (interactive-p)))

(defalias 'ar-in-name-p-atpt 'ar-name-in-p-atpt)
(defun ar-name-in-p-atpt (&optional no-delimiters)
  "Returns bounds of NAME at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'name no-delimiters (interactive-p)))

(defalias 'ar-length-of-name-atpt 'ar-name-length-atpt)
(defun ar-name-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'name no-delimiters (interactive-p)))

(defalias 'ar-copy-name-atpt 'ar-name-copy-atpt)
(defun ar-name-copy-atpt (&optional no-delimiters)
  "Returns a copy of NAME at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'name no-delimiters (interactive-p)))

(defalias 'ar-delete-name-atpt 'ar-name-delete-atpt)
(defun ar-name-delete-atpt (&optional arg)
  "Deletes NAME at point if any. "
  (interactive "*p")
  (ar-th-delete 'name arg (interactive-p)))

(defalias 'ar-delete-name-in-region 'ar-name-delete-in-region)
(defun ar-name-delete-in-region (beg end)
  "Deletes NAME at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'name beg end (interactive-p)))

(defun ar-blok-name-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around name.
  Returns blok or nil if no NAME at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'name no-delimiters (interactive-p)))

(defun ar-doublebackslash-name-atpt (&optional no-delimiters)
  "Puts doubled backslashes around NAME at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'name no-delimiters (interactive-p)))

(defun ar-doubleslash-name-atpt (&optional no-delimiters)
  "Puts doubled slashes around NAME at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'name no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-name-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around NAME at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'name no-delimiters (interactive-p)))

(defun ar-comment-name-atpt (&optional no-delimiters)
  "Comments NAME at point if any. "
  (interactive "*p")
  (ar-th-comment 'name no-delimiters (interactive-p)))

(defun ar-commatize-name-atpt (&optional no-delimiters)
  "Put a comma after NAME at point if any. "
  (interactive "*p")
  (ar-th-commatize 'name no-delimiters (interactive-p)))

(defun ar-quote-name-atpt (&optional no-delimiters)
  "Put a singlequote before NAME at point if any. "
  (interactive "*p")
  (ar-th-quote 'name no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-name-atpt 'ar-name-hyphen-atpt)
;; ;; (defun ar-name-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around NAME at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'name no-delimiters (interactive-p)))

(defalias 'ar-mark-name-atpt 'ar-name-mark-atpt)
(defun ar-name-mark-atpt ()
  "Marks NAME at point if any. "
  (interactive)
  (ar-th-mark 'name))

(defalias 'ar-hide-name-atpt 'ar-name-hide-atpt)
(defun ar-name-hide-atpt ()
  "Hides NAME at point. "
  (interactive)
  (ar-th-hide 'name))

(defalias 'ar-show-name-atpt 'ar-name-show-atpt)
(defun ar-name-show-atpt ()
  "Shows hidden NAME at point. "
  (interactive)
  (ar-th-show 'name))

(defalias 'ar-hide-show-name-atpt 'ar-name-hide-show-atpt)
(defun ar-name-hide-show-atpt ()
  "Alternatively hides or shows NAME at point. "
  (interactive)
  (ar-th-hide-show 'name))

(defalias 'ar-highlight-name-atpt-mode 'ar-name-highlight-atpt-mode)

(defun ar-name-highlight-atpt-mode (&optional no-delimiters)
  "Toggles name-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'name no-delimiters (interactive-p)))

(defalias 'ar-kill-name-atpt 'ar-name-kill-atpt)
(defun ar-name-kill-atpt (&optional no-delimiters)
  "Kills NAME at point if any. "
  (interactive "*P")
  (ar-th-kill 'name no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-name-atpt 'ar-name-kill-backward-atpt)
(defun ar-name-kill-backward-atpt (&optional no-delimiters)
  "Kills NAME at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'name no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-name-atpt 'ar-name-leftrightsinglequote-atpt)
;; ;; (defun ar-name-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'name no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-name-atpt 'ar-name-parentize-atpt)
;; ;; (defun ar-name-parentize-atpt (&optional no-delimiters)
;;   "Parentizes NAME at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'name no-delimiters (interactive-p)))

(defalias 'ar-separate-name-atpt 'ar-name-separate-atpt)
(defun ar-name-separate-atpt (&optional no-delimiters)
  "Separates NAME at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'name no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-name-atpt 'ar-name-singlequote-atpt)
;; ;; (defun ar-name-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes NAME at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'name no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-name-atpt 'ar-name-triplequotedq-atpt)
(defun ar-name-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around name. "
  (interactive "*p")
  (ar-th-triplequotedq 'name no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-name-atpt 'ar-name-triplequotesq-atpt)
(defun ar-name-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around name. "
  (interactive "*p")
  (ar-th-triplequotesq 'name no-delimiters (interactive-p)))

(defun ar-underscore-name-atpt (&optional no-delimiters)
  "Put underscore char around NAME. "
  (interactive "*p")
  (ar-th-underscore 'name no-delimiters (interactive-p)))

;; (defalias 'ar-name-whitespace-atpt 'ar-whitespace-name-atpt)
;; ;; (defun ar-whitespace-name-atpt (&optional no-delimiters)
;;   "Put whitespace char around NAME. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'name nil t))

(defalias 'ar-forward-name-atpt 'ar-name-forward-atpt)
(defun ar-name-forward-atpt (&optional arg)
  "Moves forward over NAME at point if any, does nothing otherwise.
Returns end position of NAME "
  (interactive "p")
  (ar-th-forward 'name arg (interactive-p)))

(defalias 'ar-backward-name-atpt 'ar-name-backward-atpt)
(defun ar-name-backward-atpt (&optional arg)
  "Moves backward over NAME before point if any, does nothing otherwise.
Returns beginning position of NAME "
  (interactive "p")
  (ar-th-backward 'name arg (interactive-p)))

(defalias 'ar-transpose-name-atpt 'ar-name-transpose-atpt)
(defun ar-name-transpose-atpt (&optional arg)
  "Transposes NAME with NAME before point if any. "
  (interactive "*p")
  (ar-th-transpose 'name arg (interactive-p)))

(defalias 'ar-sort-name-atpt 'ar-name-sort-atpt)
(defun ar-name-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts names in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'name reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-name-atpt 'ar-name-check-atpt)
(defun ar-name-check-atpt ()
  "Return t if a NAME at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-name-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-name-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-number-atpt (&optional arg no-delimiters)
  "Returns number at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'number arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-number-atpt 'ar-number-bounds-atpt)
(defun ar-number-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of number if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'number no-delimiters (interactive-p)))

(defun ar-number-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position NUMBER at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'number no-delimiters (interactive-p)))

(defun ar-number-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'number no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-number-atpt 'ar-number-beginning-atpt)
(defun ar-number-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'number no-delimiters (interactive-p)))

(defalias 'ar-end-of-number-atpt 'ar-number-end-atpt)
(defun ar-number-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'number no-delimiters (interactive-p)))

(defalias 'ar-in-number-p-atpt 'ar-number-in-p-atpt)
(defun ar-number-in-p-atpt (&optional no-delimiters)
  "Returns bounds of NUMBER at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'number no-delimiters (interactive-p)))

(defalias 'ar-length-of-number-atpt 'ar-number-length-atpt)
(defun ar-number-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'number no-delimiters (interactive-p)))

(defalias 'ar-copy-number-atpt 'ar-number-copy-atpt)
(defun ar-number-copy-atpt (&optional no-delimiters)
  "Returns a copy of NUMBER at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'number no-delimiters (interactive-p)))

(defalias 'ar-delete-number-atpt 'ar-number-delete-atpt)
(defun ar-number-delete-atpt (&optional arg)
  "Deletes NUMBER at point if any. "
  (interactive "*p")
  (ar-th-delete 'number arg (interactive-p)))

(defalias 'ar-delete-number-in-region 'ar-number-delete-in-region)
(defun ar-number-delete-in-region (beg end)
  "Deletes NUMBER at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'number beg end (interactive-p)))

(defun ar-blok-number-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around number.
  Returns blok or nil if no NUMBER at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'number no-delimiters (interactive-p)))

(defun ar-doublebackslash-number-atpt (&optional no-delimiters)
  "Puts doubled backslashes around NUMBER at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'number no-delimiters (interactive-p)))

(defun ar-doubleslash-number-atpt (&optional no-delimiters)
  "Puts doubled slashes around NUMBER at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'number no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-number-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around NUMBER at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'number no-delimiters (interactive-p)))

(defun ar-comment-number-atpt (&optional no-delimiters)
  "Comments NUMBER at point if any. "
  (interactive "*p")
  (ar-th-comment 'number no-delimiters (interactive-p)))

(defun ar-commatize-number-atpt (&optional no-delimiters)
  "Put a comma after NUMBER at point if any. "
  (interactive "*p")
  (ar-th-commatize 'number no-delimiters (interactive-p)))

(defun ar-quote-number-atpt (&optional no-delimiters)
  "Put a singlequote before NUMBER at point if any. "
  (interactive "*p")
  (ar-th-quote 'number no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-number-atpt 'ar-number-hyphen-atpt)
;; ;; (defun ar-number-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around NUMBER at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'number no-delimiters (interactive-p)))

(defalias 'ar-mark-number-atpt 'ar-number-mark-atpt)
(defun ar-number-mark-atpt ()
  "Marks NUMBER at point if any. "
  (interactive)
  (ar-th-mark 'number))

(defalias 'ar-hide-number-atpt 'ar-number-hide-atpt)
(defun ar-number-hide-atpt ()
  "Hides NUMBER at point. "
  (interactive)
  (ar-th-hide 'number))

(defalias 'ar-show-number-atpt 'ar-number-show-atpt)
(defun ar-number-show-atpt ()
  "Shows hidden NUMBER at point. "
  (interactive)
  (ar-th-show 'number))

(defalias 'ar-hide-show-number-atpt 'ar-number-hide-show-atpt)
(defun ar-number-hide-show-atpt ()
  "Alternatively hides or shows NUMBER at point. "
  (interactive)
  (ar-th-hide-show 'number))

(defalias 'ar-highlight-number-atpt-mode 'ar-number-highlight-atpt-mode)

(defun ar-number-highlight-atpt-mode (&optional no-delimiters)
  "Toggles number-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'number no-delimiters (interactive-p)))

(defalias 'ar-kill-number-atpt 'ar-number-kill-atpt)
(defun ar-number-kill-atpt (&optional no-delimiters)
  "Kills NUMBER at point if any. "
  (interactive "*P")
  (ar-th-kill 'number no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-number-atpt 'ar-number-kill-backward-atpt)
(defun ar-number-kill-backward-atpt (&optional no-delimiters)
  "Kills NUMBER at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'number no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-number-atpt 'ar-number-leftrightsinglequote-atpt)
;; ;; (defun ar-number-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'number no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-number-atpt 'ar-number-parentize-atpt)
;; ;; (defun ar-number-parentize-atpt (&optional no-delimiters)
;;   "Parentizes NUMBER at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'number no-delimiters (interactive-p)))

(defalias 'ar-separate-number-atpt 'ar-number-separate-atpt)
(defun ar-number-separate-atpt (&optional no-delimiters)
  "Separates NUMBER at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'number no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-number-atpt 'ar-number-singlequote-atpt)
;; ;; (defun ar-number-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes NUMBER at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'number no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-number-atpt 'ar-number-triplequotedq-atpt)
(defun ar-number-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around number. "
  (interactive "*p")
  (ar-th-triplequotedq 'number no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-number-atpt 'ar-number-triplequotesq-atpt)
(defun ar-number-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around number. "
  (interactive "*p")
  (ar-th-triplequotesq 'number no-delimiters (interactive-p)))

(defun ar-underscore-number-atpt (&optional no-delimiters)
  "Put underscore char around NUMBER. "
  (interactive "*p")
  (ar-th-underscore 'number no-delimiters (interactive-p)))

;; (defalias 'ar-number-whitespace-atpt 'ar-whitespace-number-atpt)
;; ;; (defun ar-whitespace-number-atpt (&optional no-delimiters)
;;   "Put whitespace char around NUMBER. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'number nil t))

(defalias 'ar-forward-number-atpt 'ar-number-forward-atpt)
(defun ar-number-forward-atpt (&optional arg)
  "Moves forward over NUMBER at point if any, does nothing otherwise.
Returns end position of NUMBER "
  (interactive "p")
  (ar-th-forward 'number arg (interactive-p)))

(defalias 'ar-backward-number-atpt 'ar-number-backward-atpt)
(defun ar-number-backward-atpt (&optional arg)
  "Moves backward over NUMBER before point if any, does nothing otherwise.
Returns beginning position of NUMBER "
  (interactive "p")
  (ar-th-backward 'number arg (interactive-p)))

(defalias 'ar-transpose-number-atpt 'ar-number-transpose-atpt)
(defun ar-number-transpose-atpt (&optional arg)
  "Transposes NUMBER with NUMBER before point if any. "
  (interactive "*p")
  (ar-th-transpose 'number arg (interactive-p)))

(defalias 'ar-sort-number-atpt 'ar-number-sort-atpt)
(defun ar-number-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts numbers in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'number reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-number-atpt 'ar-number-check-atpt)
(defun ar-number-check-atpt ()
  "Return t if a NUMBER at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-number-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-number-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-page-atpt (&optional arg no-delimiters)
  "Returns page at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'page arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-page-atpt 'ar-page-bounds-atpt)
(defun ar-page-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of page if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'page no-delimiters (interactive-p)))

(defun ar-page-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position PAGE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'page no-delimiters (interactive-p)))

(defun ar-page-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'page no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-page-atpt 'ar-page-beginning-atpt)
(defun ar-page-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'page no-delimiters (interactive-p)))

(defalias 'ar-end-of-page-atpt 'ar-page-end-atpt)
(defun ar-page-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'page no-delimiters (interactive-p)))

(defalias 'ar-in-page-p-atpt 'ar-page-in-p-atpt)
(defun ar-page-in-p-atpt (&optional no-delimiters)
  "Returns bounds of PAGE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'page no-delimiters (interactive-p)))

(defalias 'ar-length-of-page-atpt 'ar-page-length-atpt)
(defun ar-page-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'page no-delimiters (interactive-p)))

(defalias 'ar-copy-page-atpt 'ar-page-copy-atpt)
(defun ar-page-copy-atpt (&optional no-delimiters)
  "Returns a copy of PAGE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'page no-delimiters (interactive-p)))

(defalias 'ar-delete-page-atpt 'ar-page-delete-atpt)
(defun ar-page-delete-atpt (&optional arg)
  "Deletes PAGE at point if any. "
  (interactive "*p")
  (ar-th-delete 'page arg (interactive-p)))

(defalias 'ar-delete-page-in-region 'ar-page-delete-in-region)
(defun ar-page-delete-in-region (beg end)
  "Deletes PAGE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'page beg end (interactive-p)))

(defun ar-blok-page-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around page.
  Returns blok or nil if no PAGE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'page no-delimiters (interactive-p)))

(defun ar-doublebackslash-page-atpt (&optional no-delimiters)
  "Puts doubled backslashes around PAGE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'page no-delimiters (interactive-p)))

(defun ar-doubleslash-page-atpt (&optional no-delimiters)
  "Puts doubled slashes around PAGE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'page no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-page-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around PAGE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'page no-delimiters (interactive-p)))

(defun ar-comment-page-atpt (&optional no-delimiters)
  "Comments PAGE at point if any. "
  (interactive "*p")
  (ar-th-comment 'page no-delimiters (interactive-p)))

(defun ar-commatize-page-atpt (&optional no-delimiters)
  "Put a comma after PAGE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'page no-delimiters (interactive-p)))

(defun ar-quote-page-atpt (&optional no-delimiters)
  "Put a singlequote before PAGE at point if any. "
  (interactive "*p")
  (ar-th-quote 'page no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-page-atpt 'ar-page-hyphen-atpt)
;; ;; (defun ar-page-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around PAGE at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'page no-delimiters (interactive-p)))

(defalias 'ar-mark-page-atpt 'ar-page-mark-atpt)
(defun ar-page-mark-atpt ()
  "Marks PAGE at point if any. "
  (interactive)
  (ar-th-mark 'page))

(defalias 'ar-hide-page-atpt 'ar-page-hide-atpt)
(defun ar-page-hide-atpt ()
  "Hides PAGE at point. "
  (interactive)
  (ar-th-hide 'page))

(defalias 'ar-show-page-atpt 'ar-page-show-atpt)
(defun ar-page-show-atpt ()
  "Shows hidden PAGE at point. "
  (interactive)
  (ar-th-show 'page))

(defalias 'ar-hide-show-page-atpt 'ar-page-hide-show-atpt)
(defun ar-page-hide-show-atpt ()
  "Alternatively hides or shows PAGE at point. "
  (interactive)
  (ar-th-hide-show 'page))

(defalias 'ar-highlight-page-atpt-mode 'ar-page-highlight-atpt-mode)

(defun ar-page-highlight-atpt-mode (&optional no-delimiters)
  "Toggles page-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'page no-delimiters (interactive-p)))

(defalias 'ar-kill-page-atpt 'ar-page-kill-atpt)
(defun ar-page-kill-atpt (&optional no-delimiters)
  "Kills PAGE at point if any. "
  (interactive "*P")
  (ar-th-kill 'page no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-page-atpt 'ar-page-kill-backward-atpt)
(defun ar-page-kill-backward-atpt (&optional no-delimiters)
  "Kills PAGE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'page no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-page-atpt 'ar-page-leftrightsinglequote-atpt)
;; ;; (defun ar-page-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'page no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-page-atpt 'ar-page-parentize-atpt)
;; ;; (defun ar-page-parentize-atpt (&optional no-delimiters)
;;   "Parentizes PAGE at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'page no-delimiters (interactive-p)))

(defalias 'ar-separate-page-atpt 'ar-page-separate-atpt)
(defun ar-page-separate-atpt (&optional no-delimiters)
  "Separates PAGE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'page no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-page-atpt 'ar-page-singlequote-atpt)
;; ;; (defun ar-page-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes PAGE at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'page no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-page-atpt 'ar-page-triplequotedq-atpt)
(defun ar-page-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around page. "
  (interactive "*p")
  (ar-th-triplequotedq 'page no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-page-atpt 'ar-page-triplequotesq-atpt)
(defun ar-page-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around page. "
  (interactive "*p")
  (ar-th-triplequotesq 'page no-delimiters (interactive-p)))

(defun ar-underscore-page-atpt (&optional no-delimiters)
  "Put underscore char around PAGE. "
  (interactive "*p")
  (ar-th-underscore 'page no-delimiters (interactive-p)))

;; (defalias 'ar-page-whitespace-atpt 'ar-whitespace-page-atpt)
;; ;; (defun ar-whitespace-page-atpt (&optional no-delimiters)
;;   "Put whitespace char around PAGE. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'page nil t))

(defalias 'ar-forward-page-atpt 'ar-page-forward-atpt)
(defun ar-page-forward-atpt (&optional arg)
  "Moves forward over PAGE at point if any, does nothing otherwise.
Returns end position of PAGE "
  (interactive "p")
  (ar-th-forward 'page arg (interactive-p)))

(defalias 'ar-backward-page-atpt 'ar-page-backward-atpt)
(defun ar-page-backward-atpt (&optional arg)
  "Moves backward over PAGE before point if any, does nothing otherwise.
Returns beginning position of PAGE "
  (interactive "p")
  (ar-th-backward 'page arg (interactive-p)))

(defalias 'ar-transpose-page-atpt 'ar-page-transpose-atpt)
(defun ar-page-transpose-atpt (&optional arg)
  "Transposes PAGE with PAGE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'page arg (interactive-p)))

(defalias 'ar-sort-page-atpt 'ar-page-sort-atpt)
(defun ar-page-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts pages in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'page reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-page-atpt 'ar-page-check-atpt)
(defun ar-page-check-atpt ()
  "Return t if a PAGE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-page-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-page-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-paragraph-atpt (&optional arg no-delimiters)
  "Returns paragraph at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'paragraph arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-paragraph-atpt 'ar-paragraph-bounds-atpt)
(defun ar-paragraph-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of paragraph if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'paragraph no-delimiters (interactive-p)))

(defun ar-paragraph-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position PARAGRAPH at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'paragraph no-delimiters (interactive-p)))

(defun ar-paragraph-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-paragraph-atpt 'ar-paragraph-beginning-atpt)
(defun ar-paragraph-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-end-of-paragraph-atpt 'ar-paragraph-end-atpt)
(defun ar-paragraph-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-in-paragraph-p-atpt 'ar-paragraph-in-p-atpt)
(defun ar-paragraph-in-p-atpt (&optional no-delimiters)
  "Returns bounds of PARAGRAPH at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-length-of-paragraph-atpt 'ar-paragraph-length-atpt)
(defun ar-paragraph-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-copy-paragraph-atpt 'ar-paragraph-copy-atpt)
(defun ar-paragraph-copy-atpt (&optional no-delimiters)
  "Returns a copy of PARAGRAPH at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-delete-paragraph-atpt 'ar-paragraph-delete-atpt)
(defun ar-paragraph-delete-atpt (&optional arg)
  "Deletes PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-delete 'paragraph arg (interactive-p)))

(defalias 'ar-delete-paragraph-in-region 'ar-paragraph-delete-in-region)
(defun ar-paragraph-delete-in-region (beg end)
  "Deletes PARAGRAPH at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'paragraph beg end (interactive-p)))

(defun ar-blok-paragraph-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around paragraph.
  Returns blok or nil if no PARAGRAPH at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'paragraph no-delimiters (interactive-p)))

(defun ar-doublebackslash-paragraph-atpt (&optional no-delimiters)
  "Puts doubled backslashes around PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'paragraph no-delimiters (interactive-p)))

(defun ar-doubleslash-paragraph-atpt (&optional no-delimiters)
  "Puts doubled slashes around PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'paragraph no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-paragraph-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'paragraph no-delimiters (interactive-p)))

(defun ar-comment-paragraph-atpt (&optional no-delimiters)
  "Comments PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-comment 'paragraph no-delimiters (interactive-p)))

(defun ar-commatize-paragraph-atpt (&optional no-delimiters)
  "Put a comma after PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-commatize 'paragraph no-delimiters (interactive-p)))

(defun ar-quote-paragraph-atpt (&optional no-delimiters)
  "Put a singlequote before PARAGRAPH at point if any. "
  (interactive "*p")
  (ar-th-quote 'paragraph no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-paragraph-atpt 'ar-paragraph-hyphen-atpt)
;; ;; (defun ar-paragraph-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around PARAGRAPH at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-mark-paragraph-atpt 'ar-paragraph-mark-atpt)
(defun ar-paragraph-mark-atpt ()
  "Marks PARAGRAPH at point if any. "
  (interactive)
  (ar-th-mark 'paragraph))

(defalias 'ar-hide-paragraph-atpt 'ar-paragraph-hide-atpt)
(defun ar-paragraph-hide-atpt ()
  "Hides PARAGRAPH at point. "
  (interactive)
  (ar-th-hide 'paragraph))

(defalias 'ar-show-paragraph-atpt 'ar-paragraph-show-atpt)
(defun ar-paragraph-show-atpt ()
  "Shows hidden PARAGRAPH at point. "
  (interactive)
  (ar-th-show 'paragraph))

(defalias 'ar-hide-show-paragraph-atpt 'ar-paragraph-hide-show-atpt)
(defun ar-paragraph-hide-show-atpt ()
  "Alternatively hides or shows PARAGRAPH at point. "
  (interactive)
  (ar-th-hide-show 'paragraph))

(defalias 'ar-highlight-paragraph-atpt-mode 'ar-paragraph-highlight-atpt-mode)

(defun ar-paragraph-highlight-atpt-mode (&optional no-delimiters)
  "Toggles paragraph-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-kill-paragraph-atpt 'ar-paragraph-kill-atpt)
(defun ar-paragraph-kill-atpt (&optional no-delimiters)
  "Kills PARAGRAPH at point if any. "
  (interactive "*P")
  (ar-th-kill 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-paragraph-atpt 'ar-paragraph-kill-backward-atpt)
(defun ar-paragraph-kill-backward-atpt (&optional no-delimiters)
  "Kills PARAGRAPH at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'paragraph no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-paragraph-atpt 'ar-paragraph-leftrightsinglequote-atpt)
;; ;; (defun ar-paragraph-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'paragraph no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-paragraph-atpt 'ar-paragraph-parentize-atpt)
;; ;; (defun ar-paragraph-parentize-atpt (&optional no-delimiters)
;;   "Parentizes PARAGRAPH at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-separate-paragraph-atpt 'ar-paragraph-separate-atpt)
(defun ar-paragraph-separate-atpt (&optional no-delimiters)
  "Separates PARAGRAPH at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'paragraph no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-paragraph-atpt 'ar-paragraph-singlequote-atpt)
;; ;; (defun ar-paragraph-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes PARAGRAPH at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-paragraph-atpt 'ar-paragraph-triplequotedq-atpt)
(defun ar-paragraph-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around paragraph. "
  (interactive "*p")
  (ar-th-triplequotedq 'paragraph no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-paragraph-atpt 'ar-paragraph-triplequotesq-atpt)
(defun ar-paragraph-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around paragraph. "
  (interactive "*p")
  (ar-th-triplequotesq 'paragraph no-delimiters (interactive-p)))

(defun ar-underscore-paragraph-atpt (&optional no-delimiters)
  "Put underscore char around PARAGRAPH. "
  (interactive "*p")
  (ar-th-underscore 'paragraph no-delimiters (interactive-p)))

;; (defalias 'ar-paragraph-whitespace-atpt 'ar-whitespace-paragraph-atpt)
;; ;; (defun ar-whitespace-paragraph-atpt (&optional no-delimiters)
;;   "Put whitespace char around PARAGRAPH. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'paragraph nil t))

(defalias 'ar-forward-paragraph-atpt 'ar-paragraph-forward-atpt)
(defun ar-paragraph-forward-atpt (&optional arg)
  "Moves forward over PARAGRAPH at point if any, does nothing otherwise.
Returns end position of PARAGRAPH "
  (interactive "p")
  (ar-th-forward 'paragraph arg (interactive-p)))

(defalias 'ar-backward-paragraph-atpt 'ar-paragraph-backward-atpt)
(defun ar-paragraph-backward-atpt (&optional arg)
  "Moves backward over PARAGRAPH before point if any, does nothing otherwise.
Returns beginning position of PARAGRAPH "
  (interactive "p")
  (ar-th-backward 'paragraph arg (interactive-p)))

(defalias 'ar-transpose-paragraph-atpt 'ar-paragraph-transpose-atpt)
(defun ar-paragraph-transpose-atpt (&optional arg)
  "Transposes PARAGRAPH with PARAGRAPH before point if any. "
  (interactive "*p")
  (ar-th-transpose 'paragraph arg (interactive-p)))

(defalias 'ar-sort-paragraph-atpt 'ar-paragraph-sort-atpt)
(defun ar-paragraph-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts paragraphs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'paragraph reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-paragraph-atpt 'ar-paragraph-check-atpt)
(defun ar-paragraph-check-atpt ()
  "Return t if a PARAGRAPH at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-paragraph-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-paragraph-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-phone-atpt (&optional arg no-delimiters)
  "Returns phone at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'phone arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-phone-atpt 'ar-phone-bounds-atpt)
(defun ar-phone-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of phone if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'phone no-delimiters (interactive-p)))

(defun ar-phone-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position PHONE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'phone no-delimiters (interactive-p)))

(defun ar-phone-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'phone no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-phone-atpt 'ar-phone-beginning-atpt)
(defun ar-phone-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'phone no-delimiters (interactive-p)))

(defalias 'ar-end-of-phone-atpt 'ar-phone-end-atpt)
(defun ar-phone-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'phone no-delimiters (interactive-p)))

(defalias 'ar-in-phone-p-atpt 'ar-phone-in-p-atpt)
(defun ar-phone-in-p-atpt (&optional no-delimiters)
  "Returns bounds of PHONE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'phone no-delimiters (interactive-p)))

(defalias 'ar-length-of-phone-atpt 'ar-phone-length-atpt)
(defun ar-phone-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'phone no-delimiters (interactive-p)))

(defalias 'ar-copy-phone-atpt 'ar-phone-copy-atpt)
(defun ar-phone-copy-atpt (&optional no-delimiters)
  "Returns a copy of PHONE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'phone no-delimiters (interactive-p)))

(defalias 'ar-delete-phone-atpt 'ar-phone-delete-atpt)
(defun ar-phone-delete-atpt (&optional arg)
  "Deletes PHONE at point if any. "
  (interactive "*p")
  (ar-th-delete 'phone arg (interactive-p)))

(defalias 'ar-delete-phone-in-region 'ar-phone-delete-in-region)
(defun ar-phone-delete-in-region (beg end)
  "Deletes PHONE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'phone beg end (interactive-p)))

(defun ar-blok-phone-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around phone.
  Returns blok or nil if no PHONE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'phone no-delimiters (interactive-p)))

(defun ar-doublebackslash-phone-atpt (&optional no-delimiters)
  "Puts doubled backslashes around PHONE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'phone no-delimiters (interactive-p)))

(defun ar-doubleslash-phone-atpt (&optional no-delimiters)
  "Puts doubled slashes around PHONE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'phone no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-phone-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around PHONE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'phone no-delimiters (interactive-p)))

(defun ar-comment-phone-atpt (&optional no-delimiters)
  "Comments PHONE at point if any. "
  (interactive "*p")
  (ar-th-comment 'phone no-delimiters (interactive-p)))

(defun ar-commatize-phone-atpt (&optional no-delimiters)
  "Put a comma after PHONE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'phone no-delimiters (interactive-p)))

(defun ar-quote-phone-atpt (&optional no-delimiters)
  "Put a singlequote before PHONE at point if any. "
  (interactive "*p")
  (ar-th-quote 'phone no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-phone-atpt 'ar-phone-hyphen-atpt)
;; ;; (defun ar-phone-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around PHONE at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'phone no-delimiters (interactive-p)))

(defalias 'ar-mark-phone-atpt 'ar-phone-mark-atpt)
(defun ar-phone-mark-atpt ()
  "Marks PHONE at point if any. "
  (interactive)
  (ar-th-mark 'phone))

(defalias 'ar-hide-phone-atpt 'ar-phone-hide-atpt)
(defun ar-phone-hide-atpt ()
  "Hides PHONE at point. "
  (interactive)
  (ar-th-hide 'phone))

(defalias 'ar-show-phone-atpt 'ar-phone-show-atpt)
(defun ar-phone-show-atpt ()
  "Shows hidden PHONE at point. "
  (interactive)
  (ar-th-show 'phone))

(defalias 'ar-hide-show-phone-atpt 'ar-phone-hide-show-atpt)
(defun ar-phone-hide-show-atpt ()
  "Alternatively hides or shows PHONE at point. "
  (interactive)
  (ar-th-hide-show 'phone))

(defalias 'ar-highlight-phone-atpt-mode 'ar-phone-highlight-atpt-mode)

(defun ar-phone-highlight-atpt-mode (&optional no-delimiters)
  "Toggles phone-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'phone no-delimiters (interactive-p)))

(defalias 'ar-kill-phone-atpt 'ar-phone-kill-atpt)
(defun ar-phone-kill-atpt (&optional no-delimiters)
  "Kills PHONE at point if any. "
  (interactive "*P")
  (ar-th-kill 'phone no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-phone-atpt 'ar-phone-kill-backward-atpt)
(defun ar-phone-kill-backward-atpt (&optional no-delimiters)
  "Kills PHONE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'phone no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-phone-atpt 'ar-phone-leftrightsinglequote-atpt)
;; ;; (defun ar-phone-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'phone no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-phone-atpt 'ar-phone-parentize-atpt)
;; ;; (defun ar-phone-parentize-atpt (&optional no-delimiters)
;;   "Parentizes PHONE at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'phone no-delimiters (interactive-p)))

(defalias 'ar-separate-phone-atpt 'ar-phone-separate-atpt)
(defun ar-phone-separate-atpt (&optional no-delimiters)
  "Separates PHONE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'phone no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-phone-atpt 'ar-phone-singlequote-atpt)
;; ;; (defun ar-phone-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes PHONE at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'phone no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-phone-atpt 'ar-phone-triplequotedq-atpt)
(defun ar-phone-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around phone. "
  (interactive "*p")
  (ar-th-triplequotedq 'phone no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-phone-atpt 'ar-phone-triplequotesq-atpt)
(defun ar-phone-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around phone. "
  (interactive "*p")
  (ar-th-triplequotesq 'phone no-delimiters (interactive-p)))

(defun ar-underscore-phone-atpt (&optional no-delimiters)
  "Put underscore char around PHONE. "
  (interactive "*p")
  (ar-th-underscore 'phone no-delimiters (interactive-p)))

;; (defalias 'ar-phone-whitespace-atpt 'ar-whitespace-phone-atpt)
;; ;; (defun ar-whitespace-phone-atpt (&optional no-delimiters)
;;   "Put whitespace char around PHONE. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'phone nil t))

(defalias 'ar-forward-phone-atpt 'ar-phone-forward-atpt)
(defun ar-phone-forward-atpt (&optional arg)
  "Moves forward over PHONE at point if any, does nothing otherwise.
Returns end position of PHONE "
  (interactive "p")
  (ar-th-forward 'phone arg (interactive-p)))

(defalias 'ar-backward-phone-atpt 'ar-phone-backward-atpt)
(defun ar-phone-backward-atpt (&optional arg)
  "Moves backward over PHONE before point if any, does nothing otherwise.
Returns beginning position of PHONE "
  (interactive "p")
  (ar-th-backward 'phone arg (interactive-p)))

(defalias 'ar-transpose-phone-atpt 'ar-phone-transpose-atpt)
(defun ar-phone-transpose-atpt (&optional arg)
  "Transposes PHONE with PHONE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'phone arg (interactive-p)))

(defalias 'ar-sort-phone-atpt 'ar-phone-sort-atpt)
(defun ar-phone-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts phones in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'phone reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-phone-atpt 'ar-phone-check-atpt)
(defun ar-phone-check-atpt ()
  "Return t if a PHONE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-phone-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-phone-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-region-atpt (&optional arg no-delimiters)
  "Returns region at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'region arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-region-atpt 'ar-region-bounds-atpt)
(defun ar-region-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of region if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'region no-delimiters (interactive-p)))

(defun ar-region-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position REGION at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'region no-delimiters (interactive-p)))

(defun ar-region-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'region no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-region-atpt 'ar-region-beginning-atpt)
(defun ar-region-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'region no-delimiters (interactive-p)))

(defalias 'ar-end-of-region-atpt 'ar-region-end-atpt)
(defun ar-region-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'region no-delimiters (interactive-p)))

(defalias 'ar-in-region-p-atpt 'ar-region-in-p-atpt)
(defun ar-region-in-p-atpt (&optional no-delimiters)
  "Returns bounds of REGION at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'region no-delimiters (interactive-p)))

(defalias 'ar-length-of-region-atpt 'ar-region-length-atpt)
(defun ar-region-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'region no-delimiters (interactive-p)))

(defalias 'ar-copy-region-atpt 'ar-region-copy-atpt)
(defun ar-region-copy-atpt (&optional no-delimiters)
  "Returns a copy of REGION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'region no-delimiters (interactive-p)))

(defalias 'ar-delete-region-atpt 'ar-region-delete-atpt)
(defun ar-region-delete-atpt (&optional arg)
  "Deletes REGION at point if any. "
  (interactive "*p")
  (ar-th-delete 'region arg (interactive-p)))

(defalias 'ar-delete-region-in-region 'ar-region-delete-in-region)
(defun ar-region-delete-in-region (beg end)
  "Deletes REGION at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'region beg end (interactive-p)))

(defun ar-blok-region-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around region.
  Returns blok or nil if no REGION at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'region no-delimiters (interactive-p)))

(defun ar-doublebackslash-region-atpt (&optional no-delimiters)
  "Puts doubled backslashes around REGION at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'region no-delimiters (interactive-p)))

(defun ar-doubleslash-region-atpt (&optional no-delimiters)
  "Puts doubled slashes around REGION at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'region no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-region-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around REGION at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'region no-delimiters (interactive-p)))

(defun ar-comment-region-atpt (&optional no-delimiters)
  "Comments REGION at point if any. "
  (interactive "*p")
  (ar-th-comment 'region no-delimiters (interactive-p)))

(defun ar-commatize-region-atpt (&optional no-delimiters)
  "Put a comma after REGION at point if any. "
  (interactive "*p")
  (ar-th-commatize 'region no-delimiters (interactive-p)))

(defun ar-quote-region-atpt (&optional no-delimiters)
  "Put a singlequote before REGION at point if any. "
  (interactive "*p")
  (ar-th-quote 'region no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-region-atpt 'ar-region-hyphen-atpt)
;; ;; (defun ar-region-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around REGION at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'region no-delimiters (interactive-p)))

(defalias 'ar-mark-region-atpt 'ar-region-mark-atpt)
(defun ar-region-mark-atpt ()
  "Marks REGION at point if any. "
  (interactive)
  (ar-th-mark 'region))

(defalias 'ar-hide-region-atpt 'ar-region-hide-atpt)
(defun ar-region-hide-atpt ()
  "Hides REGION at point. "
  (interactive)
  (ar-th-hide 'region))

(defalias 'ar-show-region-atpt 'ar-region-show-atpt)
(defun ar-region-show-atpt ()
  "Shows hidden REGION at point. "
  (interactive)
  (ar-th-show 'region))

(defalias 'ar-hide-show-region-atpt 'ar-region-hide-show-atpt)
(defun ar-region-hide-show-atpt ()
  "Alternatively hides or shows REGION at point. "
  (interactive)
  (ar-th-hide-show 'region))

(defalias 'ar-highlight-region-atpt-mode 'ar-region-highlight-atpt-mode)

(defun ar-region-highlight-atpt-mode (&optional no-delimiters)
  "Toggles region-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'region no-delimiters (interactive-p)))

(defalias 'ar-kill-region-atpt 'ar-region-kill-atpt)
(defun ar-region-kill-atpt (&optional no-delimiters)
  "Kills REGION at point if any. "
  (interactive "*P")
  (ar-th-kill 'region no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-region-atpt 'ar-region-kill-backward-atpt)
(defun ar-region-kill-backward-atpt (&optional no-delimiters)
  "Kills REGION at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'region no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-region-atpt 'ar-region-leftrightsinglequote-atpt)
;; ;; (defun ar-region-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'region no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-region-atpt 'ar-region-parentize-atpt)
;; ;; (defun ar-region-parentize-atpt (&optional no-delimiters)
;;   "Parentizes REGION at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'region no-delimiters (interactive-p)))

(defalias 'ar-separate-region-atpt 'ar-region-separate-atpt)
(defun ar-region-separate-atpt (&optional no-delimiters)
  "Separates REGION at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'region no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-region-atpt 'ar-region-singlequote-atpt)
;; ;; (defun ar-region-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes REGION at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'region no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-region-atpt 'ar-region-triplequotedq-atpt)
(defun ar-region-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around region. "
  (interactive "*p")
  (ar-th-triplequotedq 'region no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-region-atpt 'ar-region-triplequotesq-atpt)
(defun ar-region-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around region. "
  (interactive "*p")
  (ar-th-triplequotesq 'region no-delimiters (interactive-p)))

(defun ar-underscore-region-atpt (&optional no-delimiters)
  "Put underscore char around REGION. "
  (interactive "*p")
  (ar-th-underscore 'region no-delimiters (interactive-p)))

;; (defalias 'ar-region-whitespace-atpt 'ar-whitespace-region-atpt)
;; ;; (defun ar-whitespace-region-atpt (&optional no-delimiters)
;;   "Put whitespace char around REGION. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'region nil t))

(defalias 'ar-forward-region-atpt 'ar-region-forward-atpt)
(defun ar-region-forward-atpt (&optional arg)
  "Moves forward over REGION at point if any, does nothing otherwise.
Returns end position of REGION "
  (interactive "p")
  (ar-th-forward 'region arg (interactive-p)))

(defalias 'ar-backward-region-atpt 'ar-region-backward-atpt)
(defun ar-region-backward-atpt (&optional arg)
  "Moves backward over REGION before point if any, does nothing otherwise.
Returns beginning position of REGION "
  (interactive "p")
  (ar-th-backward 'region arg (interactive-p)))

(defalias 'ar-transpose-region-atpt 'ar-region-transpose-atpt)
(defun ar-region-transpose-atpt (&optional arg)
  "Transposes REGION with REGION before point if any. "
  (interactive "*p")
  (ar-th-transpose 'region arg (interactive-p)))

(defalias 'ar-sort-region-atpt 'ar-region-sort-atpt)
(defun ar-region-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts regions in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'region reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-region-atpt 'ar-region-check-atpt)
(defun ar-region-check-atpt ()
  "Return t if a REGION at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-region-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-region-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-sentence-atpt (&optional arg no-delimiters)
  "Returns sentence at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'sentence arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-sentence-atpt 'ar-sentence-bounds-atpt)
(defun ar-sentence-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of sentence if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sentence no-delimiters (interactive-p)))

(defun ar-sentence-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position SENTENCE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'sentence no-delimiters (interactive-p)))

(defun ar-sentence-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'sentence no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-sentence-atpt 'ar-sentence-beginning-atpt)
(defun ar-sentence-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'sentence no-delimiters (interactive-p)))

(defalias 'ar-end-of-sentence-atpt 'ar-sentence-end-atpt)
(defun ar-sentence-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'sentence no-delimiters (interactive-p)))

(defalias 'ar-in-sentence-p-atpt 'ar-sentence-in-p-atpt)
(defun ar-sentence-in-p-atpt (&optional no-delimiters)
  "Returns bounds of SENTENCE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sentence no-delimiters (interactive-p)))

(defalias 'ar-length-of-sentence-atpt 'ar-sentence-length-atpt)
(defun ar-sentence-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'sentence no-delimiters (interactive-p)))

(defalias 'ar-copy-sentence-atpt 'ar-sentence-copy-atpt)
(defun ar-sentence-copy-atpt (&optional no-delimiters)
  "Returns a copy of SENTENCE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'sentence no-delimiters (interactive-p)))

(defalias 'ar-delete-sentence-atpt 'ar-sentence-delete-atpt)
(defun ar-sentence-delete-atpt (&optional arg)
  "Deletes SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-delete 'sentence arg (interactive-p)))

(defalias 'ar-delete-sentence-in-region 'ar-sentence-delete-in-region)
(defun ar-sentence-delete-in-region (beg end)
  "Deletes SENTENCE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'sentence beg end (interactive-p)))

(defun ar-blok-sentence-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around sentence.
  Returns blok or nil if no SENTENCE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'sentence no-delimiters (interactive-p)))

(defun ar-doublebackslash-sentence-atpt (&optional no-delimiters)
  "Puts doubled backslashes around SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'sentence no-delimiters (interactive-p)))

(defun ar-doubleslash-sentence-atpt (&optional no-delimiters)
  "Puts doubled slashes around SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'sentence no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-sentence-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'sentence no-delimiters (interactive-p)))

(defun ar-comment-sentence-atpt (&optional no-delimiters)
  "Comments SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-comment 'sentence no-delimiters (interactive-p)))

(defun ar-commatize-sentence-atpt (&optional no-delimiters)
  "Put a comma after SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'sentence no-delimiters (interactive-p)))

(defun ar-quote-sentence-atpt (&optional no-delimiters)
  "Put a singlequote before SENTENCE at point if any. "
  (interactive "*p")
  (ar-th-quote 'sentence no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-sentence-atpt 'ar-sentence-hyphen-atpt)
;; ;; (defun ar-sentence-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around SENTENCE at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'sentence no-delimiters (interactive-p)))

(defalias 'ar-mark-sentence-atpt 'ar-sentence-mark-atpt)
(defun ar-sentence-mark-atpt ()
  "Marks SENTENCE at point if any. "
  (interactive)
  (ar-th-mark 'sentence))

(defalias 'ar-hide-sentence-atpt 'ar-sentence-hide-atpt)
(defun ar-sentence-hide-atpt ()
  "Hides SENTENCE at point. "
  (interactive)
  (ar-th-hide 'sentence))

(defalias 'ar-show-sentence-atpt 'ar-sentence-show-atpt)
(defun ar-sentence-show-atpt ()
  "Shows hidden SENTENCE at point. "
  (interactive)
  (ar-th-show 'sentence))

(defalias 'ar-hide-show-sentence-atpt 'ar-sentence-hide-show-atpt)
(defun ar-sentence-hide-show-atpt ()
  "Alternatively hides or shows SENTENCE at point. "
  (interactive)
  (ar-th-hide-show 'sentence))

(defalias 'ar-highlight-sentence-atpt-mode 'ar-sentence-highlight-atpt-mode)

(defun ar-sentence-highlight-atpt-mode (&optional no-delimiters)
  "Toggles sentence-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'sentence no-delimiters (interactive-p)))

(defalias 'ar-kill-sentence-atpt 'ar-sentence-kill-atpt)
(defun ar-sentence-kill-atpt (&optional no-delimiters)
  "Kills SENTENCE at point if any. "
  (interactive "*P")
  (ar-th-kill 'sentence no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-sentence-atpt 'ar-sentence-kill-backward-atpt)
(defun ar-sentence-kill-backward-atpt (&optional no-delimiters)
  "Kills SENTENCE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'sentence no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-sentence-atpt 'ar-sentence-leftrightsinglequote-atpt)
;; ;; (defun ar-sentence-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'sentence no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-sentence-atpt 'ar-sentence-parentize-atpt)
;; ;; (defun ar-sentence-parentize-atpt (&optional no-delimiters)
;;   "Parentizes SENTENCE at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'sentence no-delimiters (interactive-p)))

(defalias 'ar-separate-sentence-atpt 'ar-sentence-separate-atpt)
(defun ar-sentence-separate-atpt (&optional no-delimiters)
  "Separates SENTENCE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'sentence no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-sentence-atpt 'ar-sentence-singlequote-atpt)
;; ;; (defun ar-sentence-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes SENTENCE at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'sentence no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-sentence-atpt 'ar-sentence-triplequotedq-atpt)
(defun ar-sentence-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around sentence. "
  (interactive "*p")
  (ar-th-triplequotedq 'sentence no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-sentence-atpt 'ar-sentence-triplequotesq-atpt)
(defun ar-sentence-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around sentence. "
  (interactive "*p")
  (ar-th-triplequotesq 'sentence no-delimiters (interactive-p)))

(defun ar-underscore-sentence-atpt (&optional no-delimiters)
  "Put underscore char around SENTENCE. "
  (interactive "*p")
  (ar-th-underscore 'sentence no-delimiters (interactive-p)))

;; (defalias 'ar-sentence-whitespace-atpt 'ar-whitespace-sentence-atpt)
;; ;; (defun ar-whitespace-sentence-atpt (&optional no-delimiters)
;;   "Put whitespace char around SENTENCE. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'sentence nil t))

(defalias 'ar-forward-sentence-atpt 'ar-sentence-forward-atpt)
(defun ar-sentence-forward-atpt (&optional arg)
  "Moves forward over SENTENCE at point if any, does nothing otherwise.
Returns end position of SENTENCE "
  (interactive "p")
  (ar-th-forward 'sentence arg (interactive-p)))

(defalias 'ar-backward-sentence-atpt 'ar-sentence-backward-atpt)
(defun ar-sentence-backward-atpt (&optional arg)
  "Moves backward over SENTENCE before point if any, does nothing otherwise.
Returns beginning position of SENTENCE "
  (interactive "p")
  (ar-th-backward 'sentence arg (interactive-p)))

(defalias 'ar-transpose-sentence-atpt 'ar-sentence-transpose-atpt)
(defun ar-sentence-transpose-atpt (&optional arg)
  "Transposes SENTENCE with SENTENCE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'sentence arg (interactive-p)))

(defalias 'ar-sort-sentence-atpt 'ar-sentence-sort-atpt)
(defun ar-sentence-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts sentences in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'sentence reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-sentence-atpt 'ar-sentence-check-atpt)
(defun ar-sentence-check-atpt ()
  "Return t if a SENTENCE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-sentence-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-sentence-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-sexp-atpt (&optional arg no-delimiters)
  "Returns sexp at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'sexp arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-sexp-atpt 'ar-sexp-bounds-atpt)
(defun ar-sexp-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of sexp if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sexp no-delimiters (interactive-p)))

(defun ar-sexp-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position SEXP at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'sexp no-delimiters (interactive-p)))

(defun ar-sexp-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'sexp no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-sexp-atpt 'ar-sexp-beginning-atpt)
(defun ar-sexp-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'sexp no-delimiters (interactive-p)))

(defalias 'ar-end-of-sexp-atpt 'ar-sexp-end-atpt)
(defun ar-sexp-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'sexp no-delimiters (interactive-p)))

(defalias 'ar-in-sexp-p-atpt 'ar-sexp-in-p-atpt)
(defun ar-sexp-in-p-atpt (&optional no-delimiters)
  "Returns bounds of SEXP at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'sexp no-delimiters (interactive-p)))

(defalias 'ar-length-of-sexp-atpt 'ar-sexp-length-atpt)
(defun ar-sexp-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'sexp no-delimiters (interactive-p)))

(defalias 'ar-copy-sexp-atpt 'ar-sexp-copy-atpt)
(defun ar-sexp-copy-atpt (&optional no-delimiters)
  "Returns a copy of SEXP at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'sexp no-delimiters (interactive-p)))

(defalias 'ar-delete-sexp-atpt 'ar-sexp-delete-atpt)
(defun ar-sexp-delete-atpt (&optional arg)
  "Deletes SEXP at point if any. "
  (interactive "*p")
  (ar-th-delete 'sexp arg (interactive-p)))

(defalias 'ar-delete-sexp-in-region 'ar-sexp-delete-in-region)
(defun ar-sexp-delete-in-region (beg end)
  "Deletes SEXP at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'sexp beg end (interactive-p)))

(defun ar-blok-sexp-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around sexp.
  Returns blok or nil if no SEXP at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'sexp no-delimiters (interactive-p)))

(defun ar-doublebackslash-sexp-atpt (&optional no-delimiters)
  "Puts doubled backslashes around SEXP at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'sexp no-delimiters (interactive-p)))

(defun ar-doubleslash-sexp-atpt (&optional no-delimiters)
  "Puts doubled slashes around SEXP at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'sexp no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-sexp-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around SEXP at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'sexp no-delimiters (interactive-p)))

(defun ar-comment-sexp-atpt (&optional no-delimiters)
  "Comments SEXP at point if any. "
  (interactive "*p")
  (ar-th-comment 'sexp no-delimiters (interactive-p)))

(defun ar-commatize-sexp-atpt (&optional no-delimiters)
  "Put a comma after SEXP at point if any. "
  (interactive "*p")
  (ar-th-commatize 'sexp no-delimiters (interactive-p)))

(defun ar-quote-sexp-atpt (&optional no-delimiters)
  "Put a singlequote before SEXP at point if any. "
  (interactive "*p")
  (ar-th-quote 'sexp no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-sexp-atpt 'ar-sexp-hyphen-atpt)
;; ;; (defun ar-sexp-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around SEXP at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'sexp no-delimiters (interactive-p)))

(defalias 'ar-mark-sexp-atpt 'ar-sexp-mark-atpt)
(defun ar-sexp-mark-atpt ()
  "Marks SEXP at point if any. "
  (interactive)
  (ar-th-mark 'sexp))

(defalias 'ar-hide-sexp-atpt 'ar-sexp-hide-atpt)
(defun ar-sexp-hide-atpt ()
  "Hides SEXP at point. "
  (interactive)
  (ar-th-hide 'sexp))

(defalias 'ar-show-sexp-atpt 'ar-sexp-show-atpt)
(defun ar-sexp-show-atpt ()
  "Shows hidden SEXP at point. "
  (interactive)
  (ar-th-show 'sexp))

(defalias 'ar-hide-show-sexp-atpt 'ar-sexp-hide-show-atpt)
(defun ar-sexp-hide-show-atpt ()
  "Alternatively hides or shows SEXP at point. "
  (interactive)
  (ar-th-hide-show 'sexp))

(defalias 'ar-highlight-sexp-atpt-mode 'ar-sexp-highlight-atpt-mode)

(defun ar-sexp-highlight-atpt-mode (&optional no-delimiters)
  "Toggles sexp-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'sexp no-delimiters (interactive-p)))

(defalias 'ar-kill-sexp-atpt 'ar-sexp-kill-atpt)
(defun ar-sexp-kill-atpt (&optional no-delimiters)
  "Kills SEXP at point if any. "
  (interactive "*P")
  (ar-th-kill 'sexp no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-sexp-atpt 'ar-sexp-kill-backward-atpt)
(defun ar-sexp-kill-backward-atpt (&optional no-delimiters)
  "Kills SEXP at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'sexp no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-sexp-atpt 'ar-sexp-leftrightsinglequote-atpt)
;; ;; (defun ar-sexp-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'sexp no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-sexp-atpt 'ar-sexp-parentize-atpt)
;; ;; (defun ar-sexp-parentize-atpt (&optional no-delimiters)
;;   "Parentizes SEXP at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'sexp no-delimiters (interactive-p)))

(defalias 'ar-separate-sexp-atpt 'ar-sexp-separate-atpt)
(defun ar-sexp-separate-atpt (&optional no-delimiters)
  "Separates SEXP at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'sexp no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-sexp-atpt 'ar-sexp-singlequote-atpt)
;; ;; (defun ar-sexp-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes SEXP at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'sexp no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-sexp-atpt 'ar-sexp-triplequotedq-atpt)
(defun ar-sexp-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around sexp. "
  (interactive "*p")
  (ar-th-triplequotedq 'sexp no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-sexp-atpt 'ar-sexp-triplequotesq-atpt)
(defun ar-sexp-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around sexp. "
  (interactive "*p")
  (ar-th-triplequotesq 'sexp no-delimiters (interactive-p)))

(defun ar-underscore-sexp-atpt (&optional no-delimiters)
  "Put underscore char around SEXP. "
  (interactive "*p")
  (ar-th-underscore 'sexp no-delimiters (interactive-p)))

;; (defalias 'ar-sexp-whitespace-atpt 'ar-whitespace-sexp-atpt)
;; ;; (defun ar-whitespace-sexp-atpt (&optional no-delimiters)
;;   "Put whitespace char around SEXP. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'sexp nil t))

(defalias 'ar-forward-sexp-atpt 'ar-sexp-forward-atpt)
(defun ar-sexp-forward-atpt (&optional arg)
  "Moves forward over SEXP at point if any, does nothing otherwise.
Returns end position of SEXP "
  (interactive "p")
  (ar-th-forward 'sexp arg (interactive-p)))

(defalias 'ar-backward-sexp-atpt 'ar-sexp-backward-atpt)
(defun ar-sexp-backward-atpt (&optional arg)
  "Moves backward over SEXP before point if any, does nothing otherwise.
Returns beginning position of SEXP "
  (interactive "p")
  (ar-th-backward 'sexp arg (interactive-p)))

(defalias 'ar-transpose-sexp-atpt 'ar-sexp-transpose-atpt)
(defun ar-sexp-transpose-atpt (&optional arg)
  "Transposes SEXP with SEXP before point if any. "
  (interactive "*p")
  (ar-th-transpose 'sexp arg (interactive-p)))

(defalias 'ar-sort-sexp-atpt 'ar-sexp-sort-atpt)
(defun ar-sexp-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts sexps in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'sexp reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-sexp-atpt 'ar-sexp-check-atpt)
(defun ar-sexp-check-atpt ()
  "Return t if a SEXP at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-sexp-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-sexp-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-string-atpt (&optional arg no-delimiters)
  "Returns string at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'string arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-string-atpt 'ar-string-bounds-atpt)
(defun ar-string-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of string if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'string no-delimiters (interactive-p)))

(defun ar-string-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position STRING at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'string no-delimiters (interactive-p)))

(defun ar-string-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'string no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-string-atpt 'ar-string-beginning-atpt)
(defun ar-string-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'string no-delimiters (interactive-p)))

(defalias 'ar-end-of-string-atpt 'ar-string-end-atpt)
(defun ar-string-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'string no-delimiters (interactive-p)))

(defalias 'ar-in-string-p-atpt 'ar-string-in-p-atpt)
(defun ar-string-in-p-atpt (&optional no-delimiters)
  "Returns bounds of STRING at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'string no-delimiters (interactive-p)))

(defalias 'ar-length-of-string-atpt 'ar-string-length-atpt)
(defun ar-string-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'string no-delimiters (interactive-p)))

(defalias 'ar-copy-string-atpt 'ar-string-copy-atpt)
(defun ar-string-copy-atpt (&optional no-delimiters)
  "Returns a copy of STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'string no-delimiters (interactive-p)))

(defalias 'ar-delete-string-atpt 'ar-string-delete-atpt)
(defun ar-string-delete-atpt (&optional arg)
  "Deletes STRING at point if any. "
  (interactive "*p")
  (ar-th-delete 'string arg (interactive-p)))

(defalias 'ar-delete-string-in-region 'ar-string-delete-in-region)
(defun ar-string-delete-in-region (beg end)
  "Deletes STRING at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'string beg end (interactive-p)))

(defun ar-blok-string-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around string.
  Returns blok or nil if no STRING at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'string no-delimiters (interactive-p)))

(defun ar-doublebackslash-string-atpt (&optional no-delimiters)
  "Puts doubled backslashes around STRING at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'string no-delimiters (interactive-p)))

(defun ar-doubleslash-string-atpt (&optional no-delimiters)
  "Puts doubled slashes around STRING at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'string no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-string-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around STRING at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'string no-delimiters (interactive-p)))

(defun ar-comment-string-atpt (&optional no-delimiters)
  "Comments STRING at point if any. "
  (interactive "*p")
  (ar-th-comment 'string no-delimiters (interactive-p)))

(defun ar-commatize-string-atpt (&optional no-delimiters)
  "Put a comma after STRING at point if any. "
  (interactive "*p")
  (ar-th-commatize 'string no-delimiters (interactive-p)))

(defun ar-quote-string-atpt (&optional no-delimiters)
  "Put a singlequote before STRING at point if any. "
  (interactive "*p")
  (ar-th-quote 'string no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-string-atpt 'ar-string-hyphen-atpt)
;; ;; (defun ar-string-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around STRING at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'string no-delimiters (interactive-p)))

(defalias 'ar-mark-string-atpt 'ar-string-mark-atpt)
(defun ar-string-mark-atpt ()
  "Marks STRING at point if any. "
  (interactive)
  (ar-th-mark 'string))

(defalias 'ar-hide-string-atpt 'ar-string-hide-atpt)
(defun ar-string-hide-atpt ()
  "Hides STRING at point. "
  (interactive)
  (ar-th-hide 'string))

(defalias 'ar-show-string-atpt 'ar-string-show-atpt)
(defun ar-string-show-atpt ()
  "Shows hidden STRING at point. "
  (interactive)
  (ar-th-show 'string))

(defalias 'ar-hide-show-string-atpt 'ar-string-hide-show-atpt)
(defun ar-string-hide-show-atpt ()
  "Alternatively hides or shows STRING at point. "
  (interactive)
  (ar-th-hide-show 'string))

(defalias 'ar-highlight-string-atpt-mode 'ar-string-highlight-atpt-mode)

(defun ar-string-highlight-atpt-mode (&optional no-delimiters)
  "Toggles string-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'string no-delimiters (interactive-p)))

(defalias 'ar-kill-string-atpt 'ar-string-kill-atpt)
(defun ar-string-kill-atpt (&optional no-delimiters)
  "Kills STRING at point if any. "
  (interactive "*P")
  (ar-th-kill 'string no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-string-atpt 'ar-string-kill-backward-atpt)
(defun ar-string-kill-backward-atpt (&optional no-delimiters)
  "Kills STRING at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'string no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-string-atpt 'ar-string-leftrightsinglequote-atpt)
;; ;; (defun ar-string-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'string no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-string-atpt 'ar-string-parentize-atpt)
;; ;; (defun ar-string-parentize-atpt (&optional no-delimiters)
;;   "Parentizes STRING at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'string no-delimiters (interactive-p)))

(defalias 'ar-separate-string-atpt 'ar-string-separate-atpt)
(defun ar-string-separate-atpt (&optional no-delimiters)
  "Separates STRING at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'string no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-string-atpt 'ar-string-singlequote-atpt)
;; ;; (defun ar-string-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes STRING at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'string no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-string-atpt 'ar-string-triplequotedq-atpt)
(defun ar-string-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around string. "
  (interactive "*p")
  (ar-th-triplequotedq 'string no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-string-atpt 'ar-string-triplequotesq-atpt)
(defun ar-string-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around string. "
  (interactive "*p")
  (ar-th-triplequotesq 'string no-delimiters (interactive-p)))

(defun ar-underscore-string-atpt (&optional no-delimiters)
  "Put underscore char around STRING. "
  (interactive "*p")
  (ar-th-underscore 'string no-delimiters (interactive-p)))

;; (defalias 'ar-string-whitespace-atpt 'ar-whitespace-string-atpt)
;; ;; (defun ar-whitespace-string-atpt (&optional no-delimiters)
;;   "Put whitespace char around STRING. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'string nil t))

(defalias 'ar-forward-string-atpt 'ar-string-forward-atpt)
(defun ar-string-forward-atpt (&optional arg)
  "Moves forward over STRING at point if any, does nothing otherwise.
Returns end position of STRING "
  (interactive "p")
  (ar-th-forward 'string arg (interactive-p)))

(defalias 'ar-backward-string-atpt 'ar-string-backward-atpt)
(defun ar-string-backward-atpt (&optional arg)
  "Moves backward over STRING before point if any, does nothing otherwise.
Returns beginning position of STRING "
  (interactive "p")
  (ar-th-backward 'string arg (interactive-p)))

(defalias 'ar-transpose-string-atpt 'ar-string-transpose-atpt)
(defun ar-string-transpose-atpt (&optional arg)
  "Transposes STRING with STRING before point if any. "
  (interactive "*p")
  (ar-th-transpose 'string arg (interactive-p)))

(defalias 'ar-sort-string-atpt 'ar-string-sort-atpt)
(defun ar-string-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts strings in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'string reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-string-atpt 'ar-string-check-atpt)
(defun ar-string-check-atpt ()
  "Return t if a STRING at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-string-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-string-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-shstruct-atpt (&optional arg no-delimiters)
  "Returns shstruct at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'shstruct arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-shstruct-atpt 'ar-shstruct-bounds-atpt)
(defun ar-shstruct-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of shstruct if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'shstruct no-delimiters (interactive-p)))

(defun ar-shstruct-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position SHSTRUCT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'shstruct no-delimiters (interactive-p)))

(defun ar-shstruct-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of SHSTRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'shstruct no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-shstruct-atpt 'ar-shstruct-beginning-atpt)
(defun ar-shstruct-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class SHSTRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'shstruct no-delimiters (interactive-p)))

(defalias 'ar-end-of-shstruct-atpt 'ar-shstruct-end-atpt)
(defun ar-shstruct-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class SHSTRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'shstruct no-delimiters (interactive-p)))

(defalias 'ar-in-shstruct-p-atpt 'ar-shstruct-in-p-atpt)
(defun ar-shstruct-in-p-atpt (&optional no-delimiters)
  "Returns bounds of SHSTRUCT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'shstruct no-delimiters (interactive-p)))

(defalias 'ar-length-of-shstruct-atpt 'ar-shstruct-length-atpt)
(defun ar-shstruct-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class SHSTRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'shstruct no-delimiters (interactive-p)))

(defalias 'ar-copy-shstruct-atpt 'ar-shstruct-copy-atpt)
(defun ar-shstruct-copy-atpt (&optional no-delimiters)
  "Returns a copy of SHSTRUCT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'shstruct no-delimiters (interactive-p)))

(defalias 'ar-delete-shstruct-atpt 'ar-shstruct-delete-atpt)
(defun ar-shstruct-delete-atpt (&optional arg)
  "Deletes SHSTRUCT at point if any. "
  (interactive "*p")
  (ar-th-delete 'shstruct arg (interactive-p)))

(defalias 'ar-delete-shstruct-in-region 'ar-shstruct-delete-in-region)
(defun ar-shstruct-delete-in-region (beg end)
  "Deletes SHSTRUCT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'shstruct beg end (interactive-p)))

(defun ar-blok-shstruct-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around shstruct.
  Returns blok or nil if no SHSTRUCT at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'shstruct no-delimiters (interactive-p)))

(defun ar-doublebackslash-shstruct-atpt (&optional no-delimiters)
  "Puts doubled backslashes around SHSTRUCT at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'shstruct no-delimiters (interactive-p)))

(defun ar-doubleslash-shstruct-atpt (&optional no-delimiters)
  "Puts doubled slashes around SHSTRUCT at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'shstruct no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-shstruct-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around SHSTRUCT at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'shstruct no-delimiters (interactive-p)))

(defun ar-comment-shstruct-atpt (&optional no-delimiters)
  "Comments SHSTRUCT at point if any. "
  (interactive "*p")
  (ar-th-comment 'shstruct no-delimiters (interactive-p)))

(defun ar-commatize-shstruct-atpt (&optional no-delimiters)
  "Put a comma after SHSTRUCT at point if any. "
  (interactive "*p")
  (ar-th-commatize 'shstruct no-delimiters (interactive-p)))

(defun ar-quote-shstruct-atpt (&optional no-delimiters)
  "Put a singlequote before SHSTRUCT at point if any. "
  (interactive "*p")
  (ar-th-quote 'shstruct no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-shstruct-atpt 'ar-shstruct-hyphen-atpt)
;; ;; (defun ar-shstruct-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around SHSTRUCT at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'shstruct no-delimiters (interactive-p)))

(defalias 'ar-mark-shstruct-atpt 'ar-shstruct-mark-atpt)
(defun ar-shstruct-mark-atpt ()
  "Marks SHSTRUCT at point if any. "
  (interactive)
  (ar-th-mark 'shstruct))

(defalias 'ar-hide-shstruct-atpt 'ar-shstruct-hide-atpt)
(defun ar-shstruct-hide-atpt ()
  "Hides SHSTRUCT at point. "
  (interactive)
  (ar-th-hide 'shstruct))

(defalias 'ar-show-shstruct-atpt 'ar-shstruct-show-atpt)
(defun ar-shstruct-show-atpt ()
  "Shows hidden SHSTRUCT at point. "
  (interactive)
  (ar-th-show 'shstruct))

(defalias 'ar-hide-show-shstruct-atpt 'ar-shstruct-hide-show-atpt)
(defun ar-shstruct-hide-show-atpt ()
  "Alternatively hides or shows SHSTRUCT at point. "
  (interactive)
  (ar-th-hide-show 'shstruct))

(defalias 'ar-highlight-shstruct-atpt-mode 'ar-shstruct-highlight-atpt-mode)

(defun ar-shstruct-highlight-atpt-mode (&optional no-delimiters)
  "Toggles shstruct-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'shstruct no-delimiters (interactive-p)))

(defalias 'ar-kill-shstruct-atpt 'ar-shstruct-kill-atpt)
(defun ar-shstruct-kill-atpt (&optional no-delimiters)
  "Kills SHSTRUCT at point if any. "
  (interactive "*P")
  (ar-th-kill 'shstruct no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-shstruct-atpt 'ar-shstruct-kill-backward-atpt)
(defun ar-shstruct-kill-backward-atpt (&optional no-delimiters)
  "Kills SHSTRUCT at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'shstruct no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-shstruct-atpt 'ar-shstruct-leftrightsinglequote-atpt)
;; ;; (defun ar-shstruct-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'shstruct no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-shstruct-atpt 'ar-shstruct-parentize-atpt)
;; ;; (defun ar-shstruct-parentize-atpt (&optional no-delimiters)
;;   "Parentizes SHSTRUCT at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'shstruct no-delimiters (interactive-p)))

(defalias 'ar-separate-shstruct-atpt 'ar-shstruct-separate-atpt)
(defun ar-shstruct-separate-atpt (&optional no-delimiters)
  "Separates SHSTRUCT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'shstruct no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-shstruct-atpt 'ar-shstruct-singlequote-atpt)
;; ;; (defun ar-shstruct-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes SHSTRUCT at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'shstruct no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-shstruct-atpt 'ar-shstruct-triplequotedq-atpt)
(defun ar-shstruct-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around shstruct. "
  (interactive "*p")
  (ar-th-triplequotedq 'shstruct no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-shstruct-atpt 'ar-shstruct-triplequotesq-atpt)
(defun ar-shstruct-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around shstruct. "
  (interactive "*p")
  (ar-th-triplequotesq 'shstruct no-delimiters (interactive-p)))

(defun ar-underscore-shstruct-atpt (&optional no-delimiters)
  "Put underscore char around SHSTRUCT. "
  (interactive "*p")
  (ar-th-underscore 'shstruct no-delimiters (interactive-p)))

;; (defalias 'ar-shstruct-whitespace-atpt 'ar-whitespace-shstruct-atpt)
;; ;; (defun ar-whitespace-shstruct-atpt (&optional no-delimiters)
;;   "Put whitespace char around SHSTRUCT. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'shstruct nil t))

(defalias 'ar-forward-shstruct-atpt 'ar-shstruct-forward-atpt)
(defun ar-shstruct-forward-atpt (&optional arg)
  "Moves forward over SHSTRUCT at point if any, does nothing otherwise.
Returns end position of SHSTRUCT "
  (interactive "p")
  (ar-th-forward 'shstruct arg (interactive-p)))

(defalias 'ar-backward-shstruct-atpt 'ar-shstruct-backward-atpt)
(defun ar-shstruct-backward-atpt (&optional arg)
  "Moves backward over SHSTRUCT before point if any, does nothing otherwise.
Returns beginning position of SHSTRUCT "
  (interactive "p")
  (ar-th-backward 'shstruct arg (interactive-p)))

(defalias 'ar-transpose-shstruct-atpt 'ar-shstruct-transpose-atpt)
(defun ar-shstruct-transpose-atpt (&optional arg)
  "Transposes SHSTRUCT with SHSTRUCT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'shstruct arg (interactive-p)))

(defalias 'ar-sort-shstruct-atpt 'ar-shstruct-sort-atpt)
(defun ar-shstruct-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts shstructs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'shstruct reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-shstruct-atpt 'ar-shstruct-check-atpt)
(defun ar-shstruct-check-atpt ()
  "Return t if a SHSTRUCT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-shstruct-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-shstruct-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-symbol-atpt (&optional arg no-delimiters)
  "Returns symbol at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'symbol arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-symbol-atpt 'ar-symbol-bounds-atpt)
(defun ar-symbol-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of symbol if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'symbol no-delimiters (interactive-p)))

(defun ar-symbol-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position SYMBOL at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'symbol no-delimiters (interactive-p)))

(defun ar-symbol-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'symbol no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-symbol-atpt 'ar-symbol-beginning-atpt)
(defun ar-symbol-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'symbol no-delimiters (interactive-p)))

(defalias 'ar-end-of-symbol-atpt 'ar-symbol-end-atpt)
(defun ar-symbol-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'symbol no-delimiters (interactive-p)))

(defalias 'ar-in-symbol-p-atpt 'ar-symbol-in-p-atpt)
(defun ar-symbol-in-p-atpt (&optional no-delimiters)
  "Returns bounds of SYMBOL at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'symbol no-delimiters (interactive-p)))

(defalias 'ar-length-of-symbol-atpt 'ar-symbol-length-atpt)
(defun ar-symbol-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'symbol no-delimiters (interactive-p)))

(defalias 'ar-copy-symbol-atpt 'ar-symbol-copy-atpt)
(defun ar-symbol-copy-atpt (&optional no-delimiters)
  "Returns a copy of SYMBOL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'symbol no-delimiters (interactive-p)))

(defalias 'ar-delete-symbol-atpt 'ar-symbol-delete-atpt)
(defun ar-symbol-delete-atpt (&optional arg)
  "Deletes SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-delete 'symbol arg (interactive-p)))

(defalias 'ar-delete-symbol-in-region 'ar-symbol-delete-in-region)
(defun ar-symbol-delete-in-region (beg end)
  "Deletes SYMBOL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'symbol beg end (interactive-p)))

(defun ar-blok-symbol-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around symbol.
  Returns blok or nil if no SYMBOL at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'symbol no-delimiters (interactive-p)))

(defun ar-doublebackslash-symbol-atpt (&optional no-delimiters)
  "Puts doubled backslashes around SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'symbol no-delimiters (interactive-p)))

(defun ar-doubleslash-symbol-atpt (&optional no-delimiters)
  "Puts doubled slashes around SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'symbol no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-symbol-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'symbol no-delimiters (interactive-p)))

(defun ar-comment-symbol-atpt (&optional no-delimiters)
  "Comments SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-comment 'symbol no-delimiters (interactive-p)))

(defun ar-commatize-symbol-atpt (&optional no-delimiters)
  "Put a comma after SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-commatize 'symbol no-delimiters (interactive-p)))

(defun ar-quote-symbol-atpt (&optional no-delimiters)
  "Put a singlequote before SYMBOL at point if any. "
  (interactive "*p")
  (ar-th-quote 'symbol no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-symbol-atpt 'ar-symbol-hyphen-atpt)
;; ;; (defun ar-symbol-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around SYMBOL at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'symbol no-delimiters (interactive-p)))

(defalias 'ar-mark-symbol-atpt 'ar-symbol-mark-atpt)
(defun ar-symbol-mark-atpt ()
  "Marks SYMBOL at point if any. "
  (interactive)
  (ar-th-mark 'symbol))

(defalias 'ar-hide-symbol-atpt 'ar-symbol-hide-atpt)
(defun ar-symbol-hide-atpt ()
  "Hides SYMBOL at point. "
  (interactive)
  (ar-th-hide 'symbol))

(defalias 'ar-show-symbol-atpt 'ar-symbol-show-atpt)
(defun ar-symbol-show-atpt ()
  "Shows hidden SYMBOL at point. "
  (interactive)
  (ar-th-show 'symbol))

(defalias 'ar-hide-show-symbol-atpt 'ar-symbol-hide-show-atpt)
(defun ar-symbol-hide-show-atpt ()
  "Alternatively hides or shows SYMBOL at point. "
  (interactive)
  (ar-th-hide-show 'symbol))

(defalias 'ar-highlight-symbol-atpt-mode 'ar-symbol-highlight-atpt-mode)

(defun ar-symbol-highlight-atpt-mode (&optional no-delimiters)
  "Toggles symbol-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'symbol no-delimiters (interactive-p)))

(defalias 'ar-kill-symbol-atpt 'ar-symbol-kill-atpt)
(defun ar-symbol-kill-atpt (&optional no-delimiters)
  "Kills SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-kill 'symbol no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-symbol-atpt 'ar-symbol-kill-backward-atpt)
(defun ar-symbol-kill-backward-atpt (&optional no-delimiters)
  "Kills SYMBOL at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'symbol no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-symbol-atpt 'ar-symbol-leftrightsinglequote-atpt)
;; ;; (defun ar-symbol-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'symbol no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-symbol-atpt 'ar-symbol-parentize-atpt)
;; ;; (defun ar-symbol-parentize-atpt (&optional no-delimiters)
;;   "Parentizes SYMBOL at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'symbol no-delimiters (interactive-p)))

(defalias 'ar-separate-symbol-atpt 'ar-symbol-separate-atpt)
(defun ar-symbol-separate-atpt (&optional no-delimiters)
  "Separates SYMBOL at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'symbol no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-symbol-atpt 'ar-symbol-singlequote-atpt)
;; ;; (defun ar-symbol-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes SYMBOL at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'symbol no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-symbol-atpt 'ar-symbol-triplequotedq-atpt)
(defun ar-symbol-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around symbol. "
  (interactive "*p")
  (ar-th-triplequotedq 'symbol no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-symbol-atpt 'ar-symbol-triplequotesq-atpt)
(defun ar-symbol-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around symbol. "
  (interactive "*p")
  (ar-th-triplequotesq 'symbol no-delimiters (interactive-p)))

(defun ar-underscore-symbol-atpt (&optional no-delimiters)
  "Put underscore char around SYMBOL. "
  (interactive "*p")
  (ar-th-underscore 'symbol no-delimiters (interactive-p)))

;; (defalias 'ar-symbol-whitespace-atpt 'ar-whitespace-symbol-atpt)
;; ;; (defun ar-whitespace-symbol-atpt (&optional no-delimiters)
;;   "Put whitespace char around SYMBOL. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'symbol nil t))

(defalias 'ar-forward-symbol-atpt 'ar-symbol-forward-atpt)
(defun ar-symbol-forward-atpt (&optional arg)
  "Moves forward over SYMBOL at point if any, does nothing otherwise.
Returns end position of SYMBOL "
  (interactive "p")
  (ar-th-forward 'symbol arg (interactive-p)))

(defalias 'ar-backward-symbol-atpt 'ar-symbol-backward-atpt)
(defun ar-symbol-backward-atpt (&optional arg)
  "Moves backward over SYMBOL before point if any, does nothing otherwise.
Returns beginning position of SYMBOL "
  (interactive "p")
  (ar-th-backward 'symbol arg (interactive-p)))

(defalias 'ar-transpose-symbol-atpt 'ar-symbol-transpose-atpt)
(defun ar-symbol-transpose-atpt (&optional arg)
  "Transposes SYMBOL with SYMBOL before point if any. "
  (interactive "*p")
  (ar-th-transpose 'symbol arg (interactive-p)))

(defalias 'ar-sort-symbol-atpt 'ar-symbol-sort-atpt)
(defun ar-symbol-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts symbols in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'symbol reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-symbol-atpt 'ar-symbol-check-atpt)
(defun ar-symbol-check-atpt ()
  "Return t if a SYMBOL at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-symbol-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-symbol-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-url-atpt (&optional arg no-delimiters)
  "Returns url at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'url arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-url-atpt 'ar-url-bounds-atpt)
(defun ar-url-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of url if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'url no-delimiters (interactive-p)))

(defun ar-url-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position URL at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'url no-delimiters (interactive-p)))

(defun ar-url-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'url no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-url-atpt 'ar-url-beginning-atpt)
(defun ar-url-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'url no-delimiters (interactive-p)))

(defalias 'ar-end-of-url-atpt 'ar-url-end-atpt)
(defun ar-url-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'url no-delimiters (interactive-p)))

(defalias 'ar-in-url-p-atpt 'ar-url-in-p-atpt)
(defun ar-url-in-p-atpt (&optional no-delimiters)
  "Returns bounds of URL at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'url no-delimiters (interactive-p)))

(defalias 'ar-length-of-url-atpt 'ar-url-length-atpt)
(defun ar-url-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'url no-delimiters (interactive-p)))

(defalias 'ar-copy-url-atpt 'ar-url-copy-atpt)
(defun ar-url-copy-atpt (&optional no-delimiters)
  "Returns a copy of URL at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'url no-delimiters (interactive-p)))

(defalias 'ar-delete-url-atpt 'ar-url-delete-atpt)
(defun ar-url-delete-atpt (&optional arg)
  "Deletes URL at point if any. "
  (interactive "*p")
  (ar-th-delete 'url arg (interactive-p)))

(defalias 'ar-delete-url-in-region 'ar-url-delete-in-region)
(defun ar-url-delete-in-region (beg end)
  "Deletes URL at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'url beg end (interactive-p)))

(defun ar-blok-url-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around url.
  Returns blok or nil if no URL at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'url no-delimiters (interactive-p)))

(defun ar-doublebackslash-url-atpt (&optional no-delimiters)
  "Puts doubled backslashes around URL at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'url no-delimiters (interactive-p)))

(defun ar-doubleslash-url-atpt (&optional no-delimiters)
  "Puts doubled slashes around URL at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'url no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-url-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around URL at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'url no-delimiters (interactive-p)))

(defun ar-comment-url-atpt (&optional no-delimiters)
  "Comments URL at point if any. "
  (interactive "*p")
  (ar-th-comment 'url no-delimiters (interactive-p)))

(defun ar-commatize-url-atpt (&optional no-delimiters)
  "Put a comma after URL at point if any. "
  (interactive "*p")
  (ar-th-commatize 'url no-delimiters (interactive-p)))

(defun ar-quote-url-atpt (&optional no-delimiters)
  "Put a singlequote before URL at point if any. "
  (interactive "*p")
  (ar-th-quote 'url no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-url-atpt 'ar-url-hyphen-atpt)
;; ;; (defun ar-url-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around URL at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'url no-delimiters (interactive-p)))

(defalias 'ar-mark-url-atpt 'ar-url-mark-atpt)
(defun ar-url-mark-atpt ()
  "Marks URL at point if any. "
  (interactive)
  (ar-th-mark 'url))

(defalias 'ar-hide-url-atpt 'ar-url-hide-atpt)
(defun ar-url-hide-atpt ()
  "Hides URL at point. "
  (interactive)
  (ar-th-hide 'url))

(defalias 'ar-show-url-atpt 'ar-url-show-atpt)
(defun ar-url-show-atpt ()
  "Shows hidden URL at point. "
  (interactive)
  (ar-th-show 'url))

(defalias 'ar-hide-show-url-atpt 'ar-url-hide-show-atpt)
(defun ar-url-hide-show-atpt ()
  "Alternatively hides or shows URL at point. "
  (interactive)
  (ar-th-hide-show 'url))

(defalias 'ar-highlight-url-atpt-mode 'ar-url-highlight-atpt-mode)

(defun ar-url-highlight-atpt-mode (&optional no-delimiters)
  "Toggles url-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'url no-delimiters (interactive-p)))

(defalias 'ar-kill-url-atpt 'ar-url-kill-atpt)
(defun ar-url-kill-atpt (&optional no-delimiters)
  "Kills URL at point if any. "
  (interactive "*P")
  (ar-th-kill 'url no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-url-atpt 'ar-url-kill-backward-atpt)
(defun ar-url-kill-backward-atpt (&optional no-delimiters)
  "Kills URL at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'url no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-url-atpt 'ar-url-leftrightsinglequote-atpt)
;; ;; (defun ar-url-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'url no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-url-atpt 'ar-url-parentize-atpt)
;; ;; (defun ar-url-parentize-atpt (&optional no-delimiters)
;;   "Parentizes URL at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'url no-delimiters (interactive-p)))

(defalias 'ar-separate-url-atpt 'ar-url-separate-atpt)
(defun ar-url-separate-atpt (&optional no-delimiters)
  "Separates URL at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'url no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-url-atpt 'ar-url-singlequote-atpt)
;; ;; (defun ar-url-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes URL at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'url no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-url-atpt 'ar-url-triplequotedq-atpt)
(defun ar-url-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around url. "
  (interactive "*p")
  (ar-th-triplequotedq 'url no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-url-atpt 'ar-url-triplequotesq-atpt)
(defun ar-url-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around url. "
  (interactive "*p")
  (ar-th-triplequotesq 'url no-delimiters (interactive-p)))

(defun ar-underscore-url-atpt (&optional no-delimiters)
  "Put underscore char around URL. "
  (interactive "*p")
  (ar-th-underscore 'url no-delimiters (interactive-p)))

;; (defalias 'ar-url-whitespace-atpt 'ar-whitespace-url-atpt)
;; ;; (defun ar-whitespace-url-atpt (&optional no-delimiters)
;;   "Put whitespace char around URL. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'url nil t))

(defalias 'ar-forward-url-atpt 'ar-url-forward-atpt)
(defun ar-url-forward-atpt (&optional arg)
  "Moves forward over URL at point if any, does nothing otherwise.
Returns end position of URL "
  (interactive "p")
  (ar-th-forward 'url arg (interactive-p)))

(defalias 'ar-backward-url-atpt 'ar-url-backward-atpt)
(defun ar-url-backward-atpt (&optional arg)
  "Moves backward over URL before point if any, does nothing otherwise.
Returns beginning position of URL "
  (interactive "p")
  (ar-th-backward 'url arg (interactive-p)))

(defalias 'ar-transpose-url-atpt 'ar-url-transpose-atpt)
(defun ar-url-transpose-atpt (&optional arg)
  "Transposes URL with URL before point if any. "
  (interactive "*p")
  (ar-th-transpose 'url arg (interactive-p)))

(defalias 'ar-sort-url-atpt 'ar-url-sort-atpt)
(defun ar-url-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts urls in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'url reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-url-atpt 'ar-url-check-atpt)
(defun ar-url-check-atpt ()
  "Return t if a URL at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-url-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-url-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-word-atpt (&optional arg no-delimiters)
  "Returns word at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'word arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-word-atpt 'ar-word-bounds-atpt)
(defun ar-word-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of word if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'word no-delimiters (interactive-p)))

(defun ar-word-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position WORD at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'word no-delimiters (interactive-p)))

(defun ar-word-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'word no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-word-atpt 'ar-word-beginning-atpt)
(defun ar-word-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'word no-delimiters (interactive-p)))

(defalias 'ar-end-of-word-atpt 'ar-word-end-atpt)
(defun ar-word-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'word no-delimiters (interactive-p)))

(defalias 'ar-in-word-p-atpt 'ar-word-in-p-atpt)
(defun ar-word-in-p-atpt (&optional no-delimiters)
  "Returns bounds of WORD at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'word no-delimiters (interactive-p)))

(defalias 'ar-length-of-word-atpt 'ar-word-length-atpt)
(defun ar-word-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'word no-delimiters (interactive-p)))

(defalias 'ar-copy-word-atpt 'ar-word-copy-atpt)
(defun ar-word-copy-atpt (&optional no-delimiters)
  "Returns a copy of WORD at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'word no-delimiters (interactive-p)))

(defalias 'ar-delete-word-atpt 'ar-word-delete-atpt)
(defun ar-word-delete-atpt (&optional arg)
  "Deletes WORD at point if any. "
  (interactive "*p")
  (ar-th-delete 'word arg (interactive-p)))

(defalias 'ar-delete-word-in-region 'ar-word-delete-in-region)
(defun ar-word-delete-in-region (beg end)
  "Deletes WORD at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'word beg end (interactive-p)))

(defun ar-blok-word-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around word.
  Returns blok or nil if no WORD at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'word no-delimiters (interactive-p)))

(defun ar-doublebackslash-word-atpt (&optional no-delimiters)
  "Puts doubled backslashes around WORD at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'word no-delimiters (interactive-p)))

(defun ar-doubleslash-word-atpt (&optional no-delimiters)
  "Puts doubled slashes around WORD at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'word no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-word-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around WORD at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'word no-delimiters (interactive-p)))

(defun ar-comment-word-atpt (&optional no-delimiters)
  "Comments WORD at point if any. "
  (interactive "*p")
  (ar-th-comment 'word no-delimiters (interactive-p)))

(defun ar-commatize-word-atpt (&optional no-delimiters)
  "Put a comma after WORD at point if any. "
  (interactive "*p")
  (ar-th-commatize 'word no-delimiters (interactive-p)))

(defun ar-quote-word-atpt (&optional no-delimiters)
  "Put a singlequote before WORD at point if any. "
  (interactive "*p")
  (ar-th-quote 'word no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-word-atpt 'ar-word-hyphen-atpt)
;; ;; (defun ar-word-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around WORD at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'word no-delimiters (interactive-p)))

(defalias 'ar-mark-word-atpt 'ar-word-mark-atpt)
(defun ar-word-mark-atpt ()
  "Marks WORD at point if any. "
  (interactive)
  (ar-th-mark 'word))

(defalias 'ar-hide-word-atpt 'ar-word-hide-atpt)
(defun ar-word-hide-atpt ()
  "Hides WORD at point. "
  (interactive)
  (ar-th-hide 'word))

(defalias 'ar-show-word-atpt 'ar-word-show-atpt)
(defun ar-word-show-atpt ()
  "Shows hidden WORD at point. "
  (interactive)
  (ar-th-show 'word))

(defalias 'ar-hide-show-word-atpt 'ar-word-hide-show-atpt)
(defun ar-word-hide-show-atpt ()
  "Alternatively hides or shows WORD at point. "
  (interactive)
  (ar-th-hide-show 'word))

(defalias 'ar-highlight-word-atpt-mode 'ar-word-highlight-atpt-mode)

(defun ar-word-highlight-atpt-mode (&optional no-delimiters)
  "Toggles word-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'word no-delimiters (interactive-p)))

(defalias 'ar-kill-word-atpt 'ar-word-kill-atpt)
(defun ar-word-kill-atpt (&optional no-delimiters)
  "Kills WORD at point if any. "
  (interactive "*P")
  (ar-th-kill 'word no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-word-atpt 'ar-word-kill-backward-atpt)
(defun ar-word-kill-backward-atpt (&optional no-delimiters)
  "Kills WORD at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'word no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-word-atpt 'ar-word-leftrightsinglequote-atpt)
;; ;; (defun ar-word-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'word no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-word-atpt 'ar-word-parentize-atpt)
;; ;; (defun ar-word-parentize-atpt (&optional no-delimiters)
;;   "Parentizes WORD at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'word no-delimiters (interactive-p)))

(defalias 'ar-separate-word-atpt 'ar-word-separate-atpt)
(defun ar-word-separate-atpt (&optional no-delimiters)
  "Separates WORD at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'word no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-word-atpt 'ar-word-singlequote-atpt)
;; ;; (defun ar-word-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes WORD at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'word no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-word-atpt 'ar-word-triplequotedq-atpt)
(defun ar-word-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around word. "
  (interactive "*p")
  (ar-th-triplequotedq 'word no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-word-atpt 'ar-word-triplequotesq-atpt)
(defun ar-word-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around word. "
  (interactive "*p")
  (ar-th-triplequotesq 'word no-delimiters (interactive-p)))

(defun ar-underscore-word-atpt (&optional no-delimiters)
  "Put underscore char around WORD. "
  (interactive "*p")
  (ar-th-underscore 'word no-delimiters (interactive-p)))

;; (defalias 'ar-word-whitespace-atpt 'ar-whitespace-word-atpt)
;; ;; (defun ar-whitespace-word-atpt (&optional no-delimiters)
;;   "Put whitespace char around WORD. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'word nil t))

(defalias 'ar-forward-word-atpt 'ar-word-forward-atpt)
(defun ar-word-forward-atpt (&optional arg)
  "Moves forward over WORD at point if any, does nothing otherwise.
Returns end position of WORD "
  (interactive "p")
  (ar-th-forward 'word arg (interactive-p)))

(defalias 'ar-backward-word-atpt 'ar-word-backward-atpt)
(defun ar-word-backward-atpt (&optional arg)
  "Moves backward over WORD before point if any, does nothing otherwise.
Returns beginning position of WORD "
  (interactive "p")
  (ar-th-backward 'word arg (interactive-p)))

(defalias 'ar-transpose-word-atpt 'ar-word-transpose-atpt)
(defun ar-word-transpose-atpt (&optional arg)
  "Transposes WORD with WORD before point if any. "
  (interactive "*p")
  (ar-th-transpose 'word arg (interactive-p)))

(defalias 'ar-sort-word-atpt 'ar-word-sort-atpt)
(defun ar-word-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts words in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'word reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-word-atpt 'ar-word-check-atpt)
(defun ar-word-check-atpt ()
  "Return t if a WORD at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-word-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-word-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-wordalphaonly-atpt (&optional arg no-delimiters)
  "Returns wordalphaonly at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'wordalphaonly arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-wordalphaonly-atpt 'ar-wordalphaonly-bounds-atpt)
(defun ar-wordalphaonly-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of wordalphaonly if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'wordalphaonly no-delimiters (interactive-p)))

(defun ar-wordalphaonly-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position WORDALPHAONLY at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'wordalphaonly no-delimiters (interactive-p)))

(defun ar-wordalphaonly-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of WORDALPHAONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'wordalphaonly no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-wordalphaonly-atpt 'ar-wordalphaonly-beginning-atpt)
(defun ar-wordalphaonly-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class WORDALPHAONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'wordalphaonly no-delimiters (interactive-p)))

(defalias 'ar-end-of-wordalphaonly-atpt 'ar-wordalphaonly-end-atpt)
(defun ar-wordalphaonly-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class WORDALPHAONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'wordalphaonly no-delimiters (interactive-p)))

(defalias 'ar-in-wordalphaonly-p-atpt 'ar-wordalphaonly-in-p-atpt)
(defun ar-wordalphaonly-in-p-atpt (&optional no-delimiters)
  "Returns bounds of WORDALPHAONLY at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'wordalphaonly no-delimiters (interactive-p)))

(defalias 'ar-length-of-wordalphaonly-atpt 'ar-wordalphaonly-length-atpt)
(defun ar-wordalphaonly-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class WORDALPHAONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'wordalphaonly no-delimiters (interactive-p)))

(defalias 'ar-copy-wordalphaonly-atpt 'ar-wordalphaonly-copy-atpt)
(defun ar-wordalphaonly-copy-atpt (&optional no-delimiters)
  "Returns a copy of WORDALPHAONLY at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'wordalphaonly no-delimiters (interactive-p)))

(defalias 'ar-delete-wordalphaonly-atpt 'ar-wordalphaonly-delete-atpt)
(defun ar-wordalphaonly-delete-atpt (&optional arg)
  "Deletes WORDALPHAONLY at point if any. "
  (interactive "*p")
  (ar-th-delete 'wordalphaonly arg (interactive-p)))

(defalias 'ar-delete-wordalphaonly-in-region 'ar-wordalphaonly-delete-in-region)
(defun ar-wordalphaonly-delete-in-region (beg end)
  "Deletes WORDALPHAONLY at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'wordalphaonly beg end (interactive-p)))

(defun ar-blok-wordalphaonly-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around wordalphaonly.
  Returns blok or nil if no WORDALPHAONLY at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'wordalphaonly no-delimiters (interactive-p)))

(defun ar-doublebackslash-wordalphaonly-atpt (&optional no-delimiters)
  "Puts doubled backslashes around WORDALPHAONLY at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'wordalphaonly no-delimiters (interactive-p)))

(defun ar-doubleslash-wordalphaonly-atpt (&optional no-delimiters)
  "Puts doubled slashes around WORDALPHAONLY at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'wordalphaonly no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-wordalphaonly-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around WORDALPHAONLY at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'wordalphaonly no-delimiters (interactive-p)))

(defun ar-comment-wordalphaonly-atpt (&optional no-delimiters)
  "Comments WORDALPHAONLY at point if any. "
  (interactive "*p")
  (ar-th-comment 'wordalphaonly no-delimiters (interactive-p)))

(defun ar-commatize-wordalphaonly-atpt (&optional no-delimiters)
  "Put a comma after WORDALPHAONLY at point if any. "
  (interactive "*p")
  (ar-th-commatize 'wordalphaonly no-delimiters (interactive-p)))

(defun ar-quote-wordalphaonly-atpt (&optional no-delimiters)
  "Put a singlequote before WORDALPHAONLY at point if any. "
  (interactive "*p")
  (ar-th-quote 'wordalphaonly no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-wordalphaonly-atpt 'ar-wordalphaonly-hyphen-atpt)
;; ;; (defun ar-wordalphaonly-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around WORDALPHAONLY at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'wordalphaonly no-delimiters (interactive-p)))

(defalias 'ar-mark-wordalphaonly-atpt 'ar-wordalphaonly-mark-atpt)
(defun ar-wordalphaonly-mark-atpt ()
  "Marks WORDALPHAONLY at point if any. "
  (interactive)
  (ar-th-mark 'wordalphaonly))

(defalias 'ar-hide-wordalphaonly-atpt 'ar-wordalphaonly-hide-atpt)
(defun ar-wordalphaonly-hide-atpt ()
  "Hides WORDALPHAONLY at point. "
  (interactive)
  (ar-th-hide 'wordalphaonly))

(defalias 'ar-show-wordalphaonly-atpt 'ar-wordalphaonly-show-atpt)
(defun ar-wordalphaonly-show-atpt ()
  "Shows hidden WORDALPHAONLY at point. "
  (interactive)
  (ar-th-show 'wordalphaonly))

(defalias 'ar-hide-show-wordalphaonly-atpt 'ar-wordalphaonly-hide-show-atpt)
(defun ar-wordalphaonly-hide-show-atpt ()
  "Alternatively hides or shows WORDALPHAONLY at point. "
  (interactive)
  (ar-th-hide-show 'wordalphaonly))

(defalias 'ar-highlight-wordalphaonly-atpt-mode 'ar-wordalphaonly-highlight-atpt-mode)

(defun ar-wordalphaonly-highlight-atpt-mode (&optional no-delimiters)
  "Toggles wordalphaonly-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'wordalphaonly no-delimiters (interactive-p)))

(defalias 'ar-kill-wordalphaonly-atpt 'ar-wordalphaonly-kill-atpt)
(defun ar-wordalphaonly-kill-atpt (&optional no-delimiters)
  "Kills WORDALPHAONLY at point if any. "
  (interactive "*P")
  (ar-th-kill 'wordalphaonly no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-wordalphaonly-atpt 'ar-wordalphaonly-kill-backward-atpt)
(defun ar-wordalphaonly-kill-backward-atpt (&optional no-delimiters)
  "Kills WORDALPHAONLY at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'wordalphaonly no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-wordalphaonly-atpt 'ar-wordalphaonly-leftrightsinglequote-atpt)
;; ;; (defun ar-wordalphaonly-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'wordalphaonly no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-wordalphaonly-atpt 'ar-wordalphaonly-parentize-atpt)
;; ;; (defun ar-wordalphaonly-parentize-atpt (&optional no-delimiters)
;;   "Parentizes WORDALPHAONLY at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'wordalphaonly no-delimiters (interactive-p)))

(defalias 'ar-separate-wordalphaonly-atpt 'ar-wordalphaonly-separate-atpt)
(defun ar-wordalphaonly-separate-atpt (&optional no-delimiters)
  "Separates WORDALPHAONLY at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'wordalphaonly no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-wordalphaonly-atpt 'ar-wordalphaonly-singlequote-atpt)
;; ;; (defun ar-wordalphaonly-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes WORDALPHAONLY at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'wordalphaonly no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-wordalphaonly-atpt 'ar-wordalphaonly-triplequotedq-atpt)
(defun ar-wordalphaonly-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around wordalphaonly. "
  (interactive "*p")
  (ar-th-triplequotedq 'wordalphaonly no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-wordalphaonly-atpt 'ar-wordalphaonly-triplequotesq-atpt)
(defun ar-wordalphaonly-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around wordalphaonly. "
  (interactive "*p")
  (ar-th-triplequotesq 'wordalphaonly no-delimiters (interactive-p)))

(defun ar-underscore-wordalphaonly-atpt (&optional no-delimiters)
  "Put underscore char around WORDALPHAONLY. "
  (interactive "*p")
  (ar-th-underscore 'wordalphaonly no-delimiters (interactive-p)))

;; (defalias 'ar-wordalphaonly-whitespace-atpt 'ar-whitespace-wordalphaonly-atpt)
;; ;; (defun ar-whitespace-wordalphaonly-atpt (&optional no-delimiters)
;;   "Put whitespace char around WORDALPHAONLY. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'wordalphaonly nil t))

(defalias 'ar-forward-wordalphaonly-atpt 'ar-wordalphaonly-forward-atpt)
(defun ar-wordalphaonly-forward-atpt (&optional arg)
  "Moves forward over WORDALPHAONLY at point if any, does nothing otherwise.
Returns end position of WORDALPHAONLY "
  (interactive "p")
  (ar-th-forward 'wordalphaonly arg (interactive-p)))

(defalias 'ar-backward-wordalphaonly-atpt 'ar-wordalphaonly-backward-atpt)
(defun ar-wordalphaonly-backward-atpt (&optional arg)
  "Moves backward over WORDALPHAONLY before point if any, does nothing otherwise.
Returns beginning position of WORDALPHAONLY "
  (interactive "p")
  (ar-th-backward 'wordalphaonly arg (interactive-p)))

(defalias 'ar-transpose-wordalphaonly-atpt 'ar-wordalphaonly-transpose-atpt)
(defun ar-wordalphaonly-transpose-atpt (&optional arg)
  "Transposes WORDALPHAONLY with WORDALPHAONLY before point if any. "
  (interactive "*p")
  (ar-th-transpose 'wordalphaonly arg (interactive-p)))

(defalias 'ar-sort-wordalphaonly-atpt 'ar-wordalphaonly-sort-atpt)
(defun ar-wordalphaonly-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts wordalphaonlys in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'wordalphaonly reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-wordalphaonly-atpt 'ar-wordalphaonly-check-atpt)
(defun ar-wordalphaonly-check-atpt ()
  "Return t if a WORDALPHAONLY at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-wordalphaonly-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-wordalphaonly-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-rest-list end

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-expression-list

(defun ar-block-atpt (&optional arg no-delimiters)
  "Returns block at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'block arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-block-atpt 'ar-block-bounds-atpt)
(defun ar-block-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of block if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'block no-delimiters (interactive-p)))

(defun ar-block-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BLOCK at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'block no-delimiters (interactive-p)))

(defun ar-block-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BLOCK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'block no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-block-atpt 'ar-block-beginning-atpt)
(defun ar-block-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class BLOCK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'block no-delimiters (interactive-p)))

(defalias 'ar-end-of-block-atpt 'ar-block-end-atpt)
(defun ar-block-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class BLOCK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'block no-delimiters (interactive-p)))

(defalias 'ar-in-block-p-atpt 'ar-block-in-p-atpt)
(defun ar-block-in-p-atpt (&optional no-delimiters)
  "Returns bounds of BLOCK at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'block no-delimiters (interactive-p)))

(defalias 'ar-length-of-block-atpt 'ar-block-length-atpt)
(defun ar-block-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class BLOCK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'block no-delimiters (interactive-p)))

(defalias 'ar-copy-block-atpt 'ar-block-copy-atpt)
(defun ar-block-copy-atpt (&optional no-delimiters)
  "Returns a copy of BLOCK at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'block no-delimiters (interactive-p)))

(defalias 'ar-delete-block-atpt 'ar-block-delete-atpt)
(defun ar-block-delete-atpt (&optional arg)
  "Deletes BLOCK at point if any. "
  (interactive "*p")
  (ar-th-delete 'block arg (interactive-p)))

(defalias 'ar-delete-block-in-region 'ar-block-delete-in-region)
(defun ar-block-delete-in-region (beg end)
  "Deletes BLOCK at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'block beg end (interactive-p)))

(defun ar-blok-block-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around block.
  Returns blok or nil if no BLOCK at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'block no-delimiters (interactive-p)))

(defun ar-doublebackslash-block-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BLOCK at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'block no-delimiters (interactive-p)))

(defun ar-doubleslash-block-atpt (&optional no-delimiters)
  "Puts doubled slashes around BLOCK at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'block no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-block-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BLOCK at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'block no-delimiters (interactive-p)))

(defun ar-comment-block-atpt (&optional no-delimiters)
  "Comments BLOCK at point if any. "
  (interactive "*p")
  (ar-th-comment 'block no-delimiters (interactive-p)))

(defun ar-commatize-block-atpt (&optional no-delimiters)
  "Put a comma after BLOCK at point if any. "
  (interactive "*p")
  (ar-th-commatize 'block no-delimiters (interactive-p)))

(defun ar-quote-block-atpt (&optional no-delimiters)
  "Put a singlequote before BLOCK at point if any. "
  (interactive "*p")
  (ar-th-quote 'block no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-block-atpt 'ar-block-hyphen-atpt)
;; ;; (defun ar-block-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around BLOCK at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'block no-delimiters (interactive-p)))

(defalias 'ar-mark-block-atpt 'ar-block-mark-atpt)
(defun ar-block-mark-atpt ()
  "Marks BLOCK at point if any. "
  (interactive)
  (ar-th-mark 'block))

(defalias 'ar-hide-block-atpt 'ar-block-hide-atpt)
(defun ar-block-hide-atpt ()
  "Hides BLOCK at point. "
  (interactive)
  (ar-th-hide 'block))

(defalias 'ar-show-block-atpt 'ar-block-show-atpt)
(defun ar-block-show-atpt ()
  "Shows hidden BLOCK at point. "
  (interactive)
  (ar-th-show 'block))

(defalias 'ar-hide-show-block-atpt 'ar-block-hide-show-atpt)
(defun ar-block-hide-show-atpt ()
  "Alternatively hides or shows BLOCK at point. "
  (interactive)
  (ar-th-hide-show 'block))

(defalias 'ar-highlight-block-atpt-mode 'ar-block-highlight-atpt-mode)

(defun ar-block-highlight-atpt-mode (&optional no-delimiters)
  "Toggles block-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'block no-delimiters (interactive-p)))

(defalias 'ar-kill-block-atpt 'ar-block-kill-atpt)
(defun ar-block-kill-atpt (&optional no-delimiters)
  "Kills BLOCK at point if any. "
  (interactive "*P")
  (ar-th-kill 'block no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-block-atpt 'ar-block-kill-backward-atpt)
(defun ar-block-kill-backward-atpt (&optional no-delimiters)
  "Kills BLOCK at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'block no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-block-atpt 'ar-block-leftrightsinglequote-atpt)
;; ;; (defun ar-block-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'block no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-block-atpt 'ar-block-parentize-atpt)
;; ;; (defun ar-block-parentize-atpt (&optional no-delimiters)
;;   "Parentizes BLOCK at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'block no-delimiters (interactive-p)))

(defalias 'ar-separate-block-atpt 'ar-block-separate-atpt)
(defun ar-block-separate-atpt (&optional no-delimiters)
  "Separates BLOCK at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'block no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-block-atpt 'ar-block-singlequote-atpt)
;; ;; (defun ar-block-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes BLOCK at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'block no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-block-atpt 'ar-block-triplequotedq-atpt)
(defun ar-block-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around block. "
  (interactive "*p")
  (ar-th-triplequotedq 'block no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-block-atpt 'ar-block-triplequotesq-atpt)
(defun ar-block-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around block. "
  (interactive "*p")
  (ar-th-triplequotesq 'block no-delimiters (interactive-p)))

(defun ar-underscore-block-atpt (&optional no-delimiters)
  "Put underscore char around BLOCK. "
  (interactive "*p")
  (ar-th-underscore 'block no-delimiters (interactive-p)))

;; (defalias 'ar-block-whitespace-atpt 'ar-whitespace-block-atpt)
;; ;; (defun ar-whitespace-block-atpt (&optional no-delimiters)
;;   "Put whitespace char around BLOCK. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'block nil t))

(defalias 'ar-forward-block-atpt 'ar-block-forward-atpt)
(defun ar-block-forward-atpt (&optional arg)
  "Moves forward over BLOCK at point if any, does nothing otherwise.
Returns end position of BLOCK "
  (interactive "p")
  (ar-th-forward 'block arg (interactive-p)))

(defalias 'ar-backward-block-atpt 'ar-block-backward-atpt)
(defun ar-block-backward-atpt (&optional arg)
  "Moves backward over BLOCK before point if any, does nothing otherwise.
Returns beginning position of BLOCK "
  (interactive "p")
  (ar-th-backward 'block arg (interactive-p)))

(defalias 'ar-transpose-block-atpt 'ar-block-transpose-atpt)
(defun ar-block-transpose-atpt (&optional arg)
  "Transposes BLOCK with BLOCK before point if any. "
  (interactive "*p")
  (ar-th-transpose 'block arg (interactive-p)))

(defalias 'ar-sort-block-atpt 'ar-block-sort-atpt)
(defun ar-block-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts blocks in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'block reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-block-atpt 'ar-block-check-atpt)
(defun ar-block-check-atpt ()
  "Return t if a BLOCK at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-block-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-block-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-block-or-clause-atpt (&optional arg no-delimiters)
  "Returns block-or-clause at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'block-or-clause arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-block-or-clause-atpt 'ar-block-or-clause-bounds-atpt)
(defun ar-block-or-clause-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of block-or-clause if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'block-or-clause no-delimiters (interactive-p)))

(defun ar-block-or-clause-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position BLOCK-OR-CLAUSE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'block-or-clause no-delimiters (interactive-p)))

(defun ar-block-or-clause-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of BLOCK-OR-CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'block-or-clause no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-block-or-clause-atpt 'ar-block-or-clause-beginning-atpt)
(defun ar-block-or-clause-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class BLOCK-OR-CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'block-or-clause no-delimiters (interactive-p)))

(defalias 'ar-end-of-block-or-clause-atpt 'ar-block-or-clause-end-atpt)
(defun ar-block-or-clause-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class BLOCK-OR-CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'block-or-clause no-delimiters (interactive-p)))

(defalias 'ar-in-block-or-clause-p-atpt 'ar-block-or-clause-in-p-atpt)
(defun ar-block-or-clause-in-p-atpt (&optional no-delimiters)
  "Returns bounds of BLOCK-OR-CLAUSE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'block-or-clause no-delimiters (interactive-p)))

(defalias 'ar-length-of-block-or-clause-atpt 'ar-block-or-clause-length-atpt)
(defun ar-block-or-clause-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class BLOCK-OR-CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'block-or-clause no-delimiters (interactive-p)))

(defalias 'ar-copy-block-or-clause-atpt 'ar-block-or-clause-copy-atpt)
(defun ar-block-or-clause-copy-atpt (&optional no-delimiters)
  "Returns a copy of BLOCK-OR-CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'block-or-clause no-delimiters (interactive-p)))

(defalias 'ar-delete-block-or-clause-atpt 'ar-block-or-clause-delete-atpt)
(defun ar-block-or-clause-delete-atpt (&optional arg)
  "Deletes BLOCK-OR-CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-delete 'block-or-clause arg (interactive-p)))

(defalias 'ar-delete-block-or-clause-in-region 'ar-block-or-clause-delete-in-region)
(defun ar-block-or-clause-delete-in-region (beg end)
  "Deletes BLOCK-OR-CLAUSE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'block-or-clause beg end (interactive-p)))

(defun ar-blok-block-or-clause-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around block-or-clause.
  Returns blok or nil if no BLOCK-OR-CLAUSE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'block-or-clause no-delimiters (interactive-p)))

(defun ar-doublebackslash-block-or-clause-atpt (&optional no-delimiters)
  "Puts doubled backslashes around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'block-or-clause no-delimiters (interactive-p)))

(defun ar-doubleslash-block-or-clause-atpt (&optional no-delimiters)
  "Puts doubled slashes around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'block-or-clause no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-block-or-clause-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around BLOCK-OR-CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'block-or-clause no-delimiters (interactive-p)))

(defun ar-comment-block-or-clause-atpt (&optional no-delimiters)
  "Comments BLOCK-OR-CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-comment 'block-or-clause no-delimiters (interactive-p)))

(defun ar-commatize-block-or-clause-atpt (&optional no-delimiters)
  "Put a comma after BLOCK-OR-CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'block-or-clause no-delimiters (interactive-p)))

(defun ar-quote-block-or-clause-atpt (&optional no-delimiters)
  "Put a singlequote before BLOCK-OR-CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-quote 'block-or-clause no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-block-or-clause-atpt 'ar-block-or-clause-hyphen-atpt)
;; ;; (defun ar-block-or-clause-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around BLOCK-OR-CLAUSE at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'block-or-clause no-delimiters (interactive-p)))

(defalias 'ar-mark-block-or-clause-atpt 'ar-block-or-clause-mark-atpt)
(defun ar-block-or-clause-mark-atpt ()
  "Marks BLOCK-OR-CLAUSE at point if any. "
  (interactive)
  (ar-th-mark 'block-or-clause))

(defalias 'ar-hide-block-or-clause-atpt 'ar-block-or-clause-hide-atpt)
(defun ar-block-or-clause-hide-atpt ()
  "Hides BLOCK-OR-CLAUSE at point. "
  (interactive)
  (ar-th-hide 'block-or-clause))

(defalias 'ar-show-block-or-clause-atpt 'ar-block-or-clause-show-atpt)
(defun ar-block-or-clause-show-atpt ()
  "Shows hidden BLOCK-OR-CLAUSE at point. "
  (interactive)
  (ar-th-show 'block-or-clause))

(defalias 'ar-hide-show-block-or-clause-atpt 'ar-block-or-clause-hide-show-atpt)
(defun ar-block-or-clause-hide-show-atpt ()
  "Alternatively hides or shows BLOCK-OR-CLAUSE at point. "
  (interactive)
  (ar-th-hide-show 'block-or-clause))

(defalias 'ar-highlight-block-or-clause-atpt-mode 'ar-block-or-clause-highlight-atpt-mode)

(defun ar-block-or-clause-highlight-atpt-mode (&optional no-delimiters)
  "Toggles block-or-clause-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'block-or-clause no-delimiters (interactive-p)))

(defalias 'ar-kill-block-or-clause-atpt 'ar-block-or-clause-kill-atpt)
(defun ar-block-or-clause-kill-atpt (&optional no-delimiters)
  "Kills BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-kill 'block-or-clause no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-block-or-clause-atpt 'ar-block-or-clause-kill-backward-atpt)
(defun ar-block-or-clause-kill-backward-atpt (&optional no-delimiters)
  "Kills BLOCK-OR-CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'block-or-clause no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-block-or-clause-atpt 'ar-block-or-clause-leftrightsinglequote-atpt)
;; ;; (defun ar-block-or-clause-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'block-or-clause no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-block-or-clause-atpt 'ar-block-or-clause-parentize-atpt)
;; ;; (defun ar-block-or-clause-parentize-atpt (&optional no-delimiters)
;;   "Parentizes BLOCK-OR-CLAUSE at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'block-or-clause no-delimiters (interactive-p)))

(defalias 'ar-separate-block-or-clause-atpt 'ar-block-or-clause-separate-atpt)
(defun ar-block-or-clause-separate-atpt (&optional no-delimiters)
  "Separates BLOCK-OR-CLAUSE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'block-or-clause no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-block-or-clause-atpt 'ar-block-or-clause-singlequote-atpt)
;; ;; (defun ar-block-or-clause-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes BLOCK-OR-CLAUSE at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'block-or-clause no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-block-or-clause-atpt 'ar-block-or-clause-triplequotedq-atpt)
(defun ar-block-or-clause-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around block-or-clause. "
  (interactive "*p")
  (ar-th-triplequotedq 'block-or-clause no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-block-or-clause-atpt 'ar-block-or-clause-triplequotesq-atpt)
(defun ar-block-or-clause-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around block-or-clause. "
  (interactive "*p")
  (ar-th-triplequotesq 'block-or-clause no-delimiters (interactive-p)))

(defun ar-underscore-block-or-clause-atpt (&optional no-delimiters)
  "Put underscore char around BLOCK-OR-CLAUSE. "
  (interactive "*p")
  (ar-th-underscore 'block-or-clause no-delimiters (interactive-p)))

;; (defalias 'ar-block-or-clause-whitespace-atpt 'ar-whitespace-block-or-clause-atpt)
;; ;; (defun ar-whitespace-block-or-clause-atpt (&optional no-delimiters)
;;   "Put whitespace char around BLOCK-OR-CLAUSE. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'block-or-clause nil t))

(defalias 'ar-forward-block-or-clause-atpt 'ar-block-or-clause-forward-atpt)
(defun ar-block-or-clause-forward-atpt (&optional arg)
  "Moves forward over BLOCK-OR-CLAUSE at point if any, does nothing otherwise.
Returns end position of BLOCK-OR-CLAUSE "
  (interactive "p")
  (ar-th-forward 'block-or-clause arg (interactive-p)))

(defalias 'ar-backward-block-or-clause-atpt 'ar-block-or-clause-backward-atpt)
(defun ar-block-or-clause-backward-atpt (&optional arg)
  "Moves backward over BLOCK-OR-CLAUSE before point if any, does nothing otherwise.
Returns beginning position of BLOCK-OR-CLAUSE "
  (interactive "p")
  (ar-th-backward 'block-or-clause arg (interactive-p)))

(defalias 'ar-transpose-block-or-clause-atpt 'ar-block-or-clause-transpose-atpt)
(defun ar-block-or-clause-transpose-atpt (&optional arg)
  "Transposes BLOCK-OR-CLAUSE with BLOCK-OR-CLAUSE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'block-or-clause arg (interactive-p)))

(defalias 'ar-sort-block-or-clause-atpt 'ar-block-or-clause-sort-atpt)
(defun ar-block-or-clause-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts block-or-clauses in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'block-or-clause reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-block-or-clause-atpt 'ar-block-or-clause-check-atpt)
(defun ar-block-or-clause-check-atpt ()
  "Return t if a BLOCK-OR-CLAUSE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-block-or-clause-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-block-or-clause-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-class-atpt (&optional arg no-delimiters)
  "Returns class at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'class arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-class-atpt 'ar-class-bounds-atpt)
(defun ar-class-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of class if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'class no-delimiters (interactive-p)))

(defun ar-class-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position CLASS at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'class no-delimiters (interactive-p)))

(defun ar-class-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'class no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-class-atpt 'ar-class-beginning-atpt)
(defun ar-class-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'class no-delimiters (interactive-p)))

(defalias 'ar-end-of-class-atpt 'ar-class-end-atpt)
(defun ar-class-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'class no-delimiters (interactive-p)))

(defalias 'ar-in-class-p-atpt 'ar-class-in-p-atpt)
(defun ar-class-in-p-atpt (&optional no-delimiters)
  "Returns bounds of CLASS at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'class no-delimiters (interactive-p)))

(defalias 'ar-length-of-class-atpt 'ar-class-length-atpt)
(defun ar-class-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'class no-delimiters (interactive-p)))

(defalias 'ar-copy-class-atpt 'ar-class-copy-atpt)
(defun ar-class-copy-atpt (&optional no-delimiters)
  "Returns a copy of CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'class no-delimiters (interactive-p)))

(defalias 'ar-delete-class-atpt 'ar-class-delete-atpt)
(defun ar-class-delete-atpt (&optional arg)
  "Deletes CLASS at point if any. "
  (interactive "*p")
  (ar-th-delete 'class arg (interactive-p)))

(defalias 'ar-delete-class-in-region 'ar-class-delete-in-region)
(defun ar-class-delete-in-region (beg end)
  "Deletes CLASS at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'class beg end (interactive-p)))

(defun ar-blok-class-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around class.
  Returns blok or nil if no CLASS at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'class no-delimiters (interactive-p)))

(defun ar-doublebackslash-class-atpt (&optional no-delimiters)
  "Puts doubled backslashes around CLASS at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'class no-delimiters (interactive-p)))

(defun ar-doubleslash-class-atpt (&optional no-delimiters)
  "Puts doubled slashes around CLASS at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'class no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-class-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around CLASS at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'class no-delimiters (interactive-p)))

(defun ar-comment-class-atpt (&optional no-delimiters)
  "Comments CLASS at point if any. "
  (interactive "*p")
  (ar-th-comment 'class no-delimiters (interactive-p)))

(defun ar-commatize-class-atpt (&optional no-delimiters)
  "Put a comma after CLASS at point if any. "
  (interactive "*p")
  (ar-th-commatize 'class no-delimiters (interactive-p)))

(defun ar-quote-class-atpt (&optional no-delimiters)
  "Put a singlequote before CLASS at point if any. "
  (interactive "*p")
  (ar-th-quote 'class no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-class-atpt 'ar-class-hyphen-atpt)
;; ;; (defun ar-class-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around CLASS at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'class no-delimiters (interactive-p)))

(defalias 'ar-mark-class-atpt 'ar-class-mark-atpt)
(defun ar-class-mark-atpt ()
  "Marks CLASS at point if any. "
  (interactive)
  (ar-th-mark 'class))

(defalias 'ar-hide-class-atpt 'ar-class-hide-atpt)
(defun ar-class-hide-atpt ()
  "Hides CLASS at point. "
  (interactive)
  (ar-th-hide 'class))

(defalias 'ar-show-class-atpt 'ar-class-show-atpt)
(defun ar-class-show-atpt ()
  "Shows hidden CLASS at point. "
  (interactive)
  (ar-th-show 'class))

(defalias 'ar-hide-show-class-atpt 'ar-class-hide-show-atpt)
(defun ar-class-hide-show-atpt ()
  "Alternatively hides or shows CLASS at point. "
  (interactive)
  (ar-th-hide-show 'class))

(defalias 'ar-highlight-class-atpt-mode 'ar-class-highlight-atpt-mode)

(defun ar-class-highlight-atpt-mode (&optional no-delimiters)
  "Toggles class-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'class no-delimiters (interactive-p)))

(defalias 'ar-kill-class-atpt 'ar-class-kill-atpt)
(defun ar-class-kill-atpt (&optional no-delimiters)
  "Kills CLASS at point if any. "
  (interactive "*P")
  (ar-th-kill 'class no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-class-atpt 'ar-class-kill-backward-atpt)
(defun ar-class-kill-backward-atpt (&optional no-delimiters)
  "Kills CLASS at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'class no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-class-atpt 'ar-class-leftrightsinglequote-atpt)
;; ;; (defun ar-class-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'class no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-class-atpt 'ar-class-parentize-atpt)
;; ;; (defun ar-class-parentize-atpt (&optional no-delimiters)
;;   "Parentizes CLASS at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'class no-delimiters (interactive-p)))

(defalias 'ar-separate-class-atpt 'ar-class-separate-atpt)
(defun ar-class-separate-atpt (&optional no-delimiters)
  "Separates CLASS at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'class no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-class-atpt 'ar-class-singlequote-atpt)
;; ;; (defun ar-class-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes CLASS at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'class no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-class-atpt 'ar-class-triplequotedq-atpt)
(defun ar-class-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around class. "
  (interactive "*p")
  (ar-th-triplequotedq 'class no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-class-atpt 'ar-class-triplequotesq-atpt)
(defun ar-class-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around class. "
  (interactive "*p")
  (ar-th-triplequotesq 'class no-delimiters (interactive-p)))

(defun ar-underscore-class-atpt (&optional no-delimiters)
  "Put underscore char around CLASS. "
  (interactive "*p")
  (ar-th-underscore 'class no-delimiters (interactive-p)))

;; (defalias 'ar-class-whitespace-atpt 'ar-whitespace-class-atpt)
;; ;; (defun ar-whitespace-class-atpt (&optional no-delimiters)
;;   "Put whitespace char around CLASS. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'class nil t))

(defalias 'ar-forward-class-atpt 'ar-class-forward-atpt)
(defun ar-class-forward-atpt (&optional arg)
  "Moves forward over CLASS at point if any, does nothing otherwise.
Returns end position of CLASS "
  (interactive "p")
  (ar-th-forward 'class arg (interactive-p)))

(defalias 'ar-backward-class-atpt 'ar-class-backward-atpt)
(defun ar-class-backward-atpt (&optional arg)
  "Moves backward over CLASS before point if any, does nothing otherwise.
Returns beginning position of CLASS "
  (interactive "p")
  (ar-th-backward 'class arg (interactive-p)))

(defalias 'ar-transpose-class-atpt 'ar-class-transpose-atpt)
(defun ar-class-transpose-atpt (&optional arg)
  "Transposes CLASS with CLASS before point if any. "
  (interactive "*p")
  (ar-th-transpose 'class arg (interactive-p)))

(defalias 'ar-sort-class-atpt 'ar-class-sort-atpt)
(defun ar-class-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts classs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'class reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-class-atpt 'ar-class-check-atpt)
(defun ar-class-check-atpt ()
  "Return t if a CLASS at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-class-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-class-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-clause-atpt (&optional arg no-delimiters)
  "Returns clause at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'clause arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-clause-atpt 'ar-clause-bounds-atpt)
(defun ar-clause-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of clause if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'clause no-delimiters (interactive-p)))

(defun ar-clause-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position CLAUSE at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'clause no-delimiters (interactive-p)))

(defun ar-clause-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'clause no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-clause-atpt 'ar-clause-beginning-atpt)
(defun ar-clause-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'clause no-delimiters (interactive-p)))

(defalias 'ar-end-of-clause-atpt 'ar-clause-end-atpt)
(defun ar-clause-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'clause no-delimiters (interactive-p)))

(defalias 'ar-in-clause-p-atpt 'ar-clause-in-p-atpt)
(defun ar-clause-in-p-atpt (&optional no-delimiters)
  "Returns bounds of CLAUSE at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'clause no-delimiters (interactive-p)))

(defalias 'ar-length-of-clause-atpt 'ar-clause-length-atpt)
(defun ar-clause-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'clause no-delimiters (interactive-p)))

(defalias 'ar-copy-clause-atpt 'ar-clause-copy-atpt)
(defun ar-clause-copy-atpt (&optional no-delimiters)
  "Returns a copy of CLAUSE at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'clause no-delimiters (interactive-p)))

(defalias 'ar-delete-clause-atpt 'ar-clause-delete-atpt)
(defun ar-clause-delete-atpt (&optional arg)
  "Deletes CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-delete 'clause arg (interactive-p)))

(defalias 'ar-delete-clause-in-region 'ar-clause-delete-in-region)
(defun ar-clause-delete-in-region (beg end)
  "Deletes CLAUSE at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'clause beg end (interactive-p)))

(defun ar-blok-clause-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around clause.
  Returns blok or nil if no CLAUSE at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'clause no-delimiters (interactive-p)))

(defun ar-doublebackslash-clause-atpt (&optional no-delimiters)
  "Puts doubled backslashes around CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'clause no-delimiters (interactive-p)))

(defun ar-doubleslash-clause-atpt (&optional no-delimiters)
  "Puts doubled slashes around CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'clause no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-clause-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'clause no-delimiters (interactive-p)))

(defun ar-comment-clause-atpt (&optional no-delimiters)
  "Comments CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-comment 'clause no-delimiters (interactive-p)))

(defun ar-commatize-clause-atpt (&optional no-delimiters)
  "Put a comma after CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-commatize 'clause no-delimiters (interactive-p)))

(defun ar-quote-clause-atpt (&optional no-delimiters)
  "Put a singlequote before CLAUSE at point if any. "
  (interactive "*p")
  (ar-th-quote 'clause no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-clause-atpt 'ar-clause-hyphen-atpt)
;; ;; (defun ar-clause-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around CLAUSE at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'clause no-delimiters (interactive-p)))

(defalias 'ar-mark-clause-atpt 'ar-clause-mark-atpt)
(defun ar-clause-mark-atpt ()
  "Marks CLAUSE at point if any. "
  (interactive)
  (ar-th-mark 'clause))

(defalias 'ar-hide-clause-atpt 'ar-clause-hide-atpt)
(defun ar-clause-hide-atpt ()
  "Hides CLAUSE at point. "
  (interactive)
  (ar-th-hide 'clause))

(defalias 'ar-show-clause-atpt 'ar-clause-show-atpt)
(defun ar-clause-show-atpt ()
  "Shows hidden CLAUSE at point. "
  (interactive)
  (ar-th-show 'clause))

(defalias 'ar-hide-show-clause-atpt 'ar-clause-hide-show-atpt)
(defun ar-clause-hide-show-atpt ()
  "Alternatively hides or shows CLAUSE at point. "
  (interactive)
  (ar-th-hide-show 'clause))

(defalias 'ar-highlight-clause-atpt-mode 'ar-clause-highlight-atpt-mode)

(defun ar-clause-highlight-atpt-mode (&optional no-delimiters)
  "Toggles clause-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'clause no-delimiters (interactive-p)))

(defalias 'ar-kill-clause-atpt 'ar-clause-kill-atpt)
(defun ar-clause-kill-atpt (&optional no-delimiters)
  "Kills CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-kill 'clause no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-clause-atpt 'ar-clause-kill-backward-atpt)
(defun ar-clause-kill-backward-atpt (&optional no-delimiters)
  "Kills CLAUSE at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'clause no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-clause-atpt 'ar-clause-leftrightsinglequote-atpt)
;; ;; (defun ar-clause-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'clause no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-clause-atpt 'ar-clause-parentize-atpt)
;; ;; (defun ar-clause-parentize-atpt (&optional no-delimiters)
;;   "Parentizes CLAUSE at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'clause no-delimiters (interactive-p)))

(defalias 'ar-separate-clause-atpt 'ar-clause-separate-atpt)
(defun ar-clause-separate-atpt (&optional no-delimiters)
  "Separates CLAUSE at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'clause no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-clause-atpt 'ar-clause-singlequote-atpt)
;; ;; (defun ar-clause-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes CLAUSE at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'clause no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-clause-atpt 'ar-clause-triplequotedq-atpt)
(defun ar-clause-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around clause. "
  (interactive "*p")
  (ar-th-triplequotedq 'clause no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-clause-atpt 'ar-clause-triplequotesq-atpt)
(defun ar-clause-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around clause. "
  (interactive "*p")
  (ar-th-triplequotesq 'clause no-delimiters (interactive-p)))

(defun ar-underscore-clause-atpt (&optional no-delimiters)
  "Put underscore char around CLAUSE. "
  (interactive "*p")
  (ar-th-underscore 'clause no-delimiters (interactive-p)))

;; (defalias 'ar-clause-whitespace-atpt 'ar-whitespace-clause-atpt)
;; ;; (defun ar-whitespace-clause-atpt (&optional no-delimiters)
;;   "Put whitespace char around CLAUSE. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'clause nil t))

(defalias 'ar-forward-clause-atpt 'ar-clause-forward-atpt)
(defun ar-clause-forward-atpt (&optional arg)
  "Moves forward over CLAUSE at point if any, does nothing otherwise.
Returns end position of CLAUSE "
  (interactive "p")
  (ar-th-forward 'clause arg (interactive-p)))

(defalias 'ar-backward-clause-atpt 'ar-clause-backward-atpt)
(defun ar-clause-backward-atpt (&optional arg)
  "Moves backward over CLAUSE before point if any, does nothing otherwise.
Returns beginning position of CLAUSE "
  (interactive "p")
  (ar-th-backward 'clause arg (interactive-p)))

(defalias 'ar-transpose-clause-atpt 'ar-clause-transpose-atpt)
(defun ar-clause-transpose-atpt (&optional arg)
  "Transposes CLAUSE with CLAUSE before point if any. "
  (interactive "*p")
  (ar-th-transpose 'clause arg (interactive-p)))

(defalias 'ar-sort-clause-atpt 'ar-clause-sort-atpt)
(defun ar-clause-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts clauses in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'clause reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-clause-atpt 'ar-clause-check-atpt)
(defun ar-clause-check-atpt ()
  "Return t if a CLAUSE at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-clause-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-clause-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-def-or-class-atpt (&optional arg no-delimiters)
  "Returns def-or-class at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'def-or-class arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-def-or-class-atpt 'ar-def-or-class-bounds-atpt)
(defun ar-def-or-class-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of def-or-class if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'def-or-class no-delimiters (interactive-p)))

(defun ar-def-or-class-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DEF-OR-CLASS at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'def-or-class no-delimiters (interactive-p)))

(defun ar-def-or-class-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DEF-OR-CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'def-or-class no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-def-or-class-atpt 'ar-def-or-class-beginning-atpt)
(defun ar-def-or-class-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DEF-OR-CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'def-or-class no-delimiters (interactive-p)))

(defalias 'ar-end-of-def-or-class-atpt 'ar-def-or-class-end-atpt)
(defun ar-def-or-class-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DEF-OR-CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'def-or-class no-delimiters (interactive-p)))

(defalias 'ar-in-def-or-class-p-atpt 'ar-def-or-class-in-p-atpt)
(defun ar-def-or-class-in-p-atpt (&optional no-delimiters)
  "Returns bounds of DEF-OR-CLASS at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'def-or-class no-delimiters (interactive-p)))

(defalias 'ar-length-of-def-or-class-atpt 'ar-def-or-class-length-atpt)
(defun ar-def-or-class-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DEF-OR-CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'def-or-class no-delimiters (interactive-p)))

(defalias 'ar-copy-def-or-class-atpt 'ar-def-or-class-copy-atpt)
(defun ar-def-or-class-copy-atpt (&optional no-delimiters)
  "Returns a copy of DEF-OR-CLASS at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'def-or-class no-delimiters (interactive-p)))

(defalias 'ar-delete-def-or-class-atpt 'ar-def-or-class-delete-atpt)
(defun ar-def-or-class-delete-atpt (&optional arg)
  "Deletes DEF-OR-CLASS at point if any. "
  (interactive "*p")
  (ar-th-delete 'def-or-class arg (interactive-p)))

(defalias 'ar-delete-def-or-class-in-region 'ar-def-or-class-delete-in-region)
(defun ar-def-or-class-delete-in-region (beg end)
  "Deletes DEF-OR-CLASS at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'def-or-class beg end (interactive-p)))

(defun ar-blok-def-or-class-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around def-or-class.
  Returns blok or nil if no DEF-OR-CLASS at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'def-or-class no-delimiters (interactive-p)))

(defun ar-doublebackslash-def-or-class-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DEF-OR-CLASS at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'def-or-class no-delimiters (interactive-p)))

(defun ar-doubleslash-def-or-class-atpt (&optional no-delimiters)
  "Puts doubled slashes around DEF-OR-CLASS at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'def-or-class no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-def-or-class-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DEF-OR-CLASS at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'def-or-class no-delimiters (interactive-p)))

(defun ar-comment-def-or-class-atpt (&optional no-delimiters)
  "Comments DEF-OR-CLASS at point if any. "
  (interactive "*p")
  (ar-th-comment 'def-or-class no-delimiters (interactive-p)))

(defun ar-commatize-def-or-class-atpt (&optional no-delimiters)
  "Put a comma after DEF-OR-CLASS at point if any. "
  (interactive "*p")
  (ar-th-commatize 'def-or-class no-delimiters (interactive-p)))

(defun ar-quote-def-or-class-atpt (&optional no-delimiters)
  "Put a singlequote before DEF-OR-CLASS at point if any. "
  (interactive "*p")
  (ar-th-quote 'def-or-class no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-def-or-class-atpt 'ar-def-or-class-hyphen-atpt)
;; ;; (defun ar-def-or-class-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around DEF-OR-CLASS at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'def-or-class no-delimiters (interactive-p)))

(defalias 'ar-mark-def-or-class-atpt 'ar-def-or-class-mark-atpt)
(defun ar-def-or-class-mark-atpt ()
  "Marks DEF-OR-CLASS at point if any. "
  (interactive)
  (ar-th-mark 'def-or-class))

(defalias 'ar-hide-def-or-class-atpt 'ar-def-or-class-hide-atpt)
(defun ar-def-or-class-hide-atpt ()
  "Hides DEF-OR-CLASS at point. "
  (interactive)
  (ar-th-hide 'def-or-class))

(defalias 'ar-show-def-or-class-atpt 'ar-def-or-class-show-atpt)
(defun ar-def-or-class-show-atpt ()
  "Shows hidden DEF-OR-CLASS at point. "
  (interactive)
  (ar-th-show 'def-or-class))

(defalias 'ar-hide-show-def-or-class-atpt 'ar-def-or-class-hide-show-atpt)
(defun ar-def-or-class-hide-show-atpt ()
  "Alternatively hides or shows DEF-OR-CLASS at point. "
  (interactive)
  (ar-th-hide-show 'def-or-class))

(defalias 'ar-highlight-def-or-class-atpt-mode 'ar-def-or-class-highlight-atpt-mode)

(defun ar-def-or-class-highlight-atpt-mode (&optional no-delimiters)
  "Toggles def-or-class-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'def-or-class no-delimiters (interactive-p)))

(defalias 'ar-kill-def-or-class-atpt 'ar-def-or-class-kill-atpt)
(defun ar-def-or-class-kill-atpt (&optional no-delimiters)
  "Kills DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-kill 'def-or-class no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-def-or-class-atpt 'ar-def-or-class-kill-backward-atpt)
(defun ar-def-or-class-kill-backward-atpt (&optional no-delimiters)
  "Kills DEF-OR-CLASS at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'def-or-class no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-def-or-class-atpt 'ar-def-or-class-leftrightsinglequote-atpt)
;; ;; (defun ar-def-or-class-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'def-or-class no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-def-or-class-atpt 'ar-def-or-class-parentize-atpt)
;; ;; (defun ar-def-or-class-parentize-atpt (&optional no-delimiters)
;;   "Parentizes DEF-OR-CLASS at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'def-or-class no-delimiters (interactive-p)))

(defalias 'ar-separate-def-or-class-atpt 'ar-def-or-class-separate-atpt)
(defun ar-def-or-class-separate-atpt (&optional no-delimiters)
  "Separates DEF-OR-CLASS at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'def-or-class no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-def-or-class-atpt 'ar-def-or-class-singlequote-atpt)
;; ;; (defun ar-def-or-class-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes DEF-OR-CLASS at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'def-or-class no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-def-or-class-atpt 'ar-def-or-class-triplequotedq-atpt)
(defun ar-def-or-class-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around def-or-class. "
  (interactive "*p")
  (ar-th-triplequotedq 'def-or-class no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-def-or-class-atpt 'ar-def-or-class-triplequotesq-atpt)
(defun ar-def-or-class-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around def-or-class. "
  (interactive "*p")
  (ar-th-triplequotesq 'def-or-class no-delimiters (interactive-p)))

(defun ar-underscore-def-or-class-atpt (&optional no-delimiters)
  "Put underscore char around DEF-OR-CLASS. "
  (interactive "*p")
  (ar-th-underscore 'def-or-class no-delimiters (interactive-p)))

;; (defalias 'ar-def-or-class-whitespace-atpt 'ar-whitespace-def-or-class-atpt)
;; ;; (defun ar-whitespace-def-or-class-atpt (&optional no-delimiters)
;;   "Put whitespace char around DEF-OR-CLASS. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'def-or-class nil t))

(defalias 'ar-forward-def-or-class-atpt 'ar-def-or-class-forward-atpt)
(defun ar-def-or-class-forward-atpt (&optional arg)
  "Moves forward over DEF-OR-CLASS at point if any, does nothing otherwise.
Returns end position of DEF-OR-CLASS "
  (interactive "p")
  (ar-th-forward 'def-or-class arg (interactive-p)))

(defalias 'ar-backward-def-or-class-atpt 'ar-def-or-class-backward-atpt)
(defun ar-def-or-class-backward-atpt (&optional arg)
  "Moves backward over DEF-OR-CLASS before point if any, does nothing otherwise.
Returns beginning position of DEF-OR-CLASS "
  (interactive "p")
  (ar-th-backward 'def-or-class arg (interactive-p)))

(defalias 'ar-transpose-def-or-class-atpt 'ar-def-or-class-transpose-atpt)
(defun ar-def-or-class-transpose-atpt (&optional arg)
  "Transposes DEF-OR-CLASS with DEF-OR-CLASS before point if any. "
  (interactive "*p")
  (ar-th-transpose 'def-or-class arg (interactive-p)))

(defalias 'ar-sort-def-or-class-atpt 'ar-def-or-class-sort-atpt)
(defun ar-def-or-class-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts def-or-classs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'def-or-class reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-def-or-class-atpt 'ar-def-or-class-check-atpt)
(defun ar-def-or-class-check-atpt ()
  "Return t if a DEF-OR-CLASS at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-def-or-class-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-def-or-class-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-def-atpt (&optional arg no-delimiters)
  "Returns def at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'def arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-def-atpt 'ar-def-bounds-atpt)
(defun ar-def-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of def if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'def no-delimiters (interactive-p)))

(defun ar-def-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position DEF at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'def no-delimiters (interactive-p)))

(defun ar-def-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of DEF at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'def no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-def-atpt 'ar-def-beginning-atpt)
(defun ar-def-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class DEF at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'def no-delimiters (interactive-p)))

(defalias 'ar-end-of-def-atpt 'ar-def-end-atpt)
(defun ar-def-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class DEF at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'def no-delimiters (interactive-p)))

(defalias 'ar-in-def-p-atpt 'ar-def-in-p-atpt)
(defun ar-def-in-p-atpt (&optional no-delimiters)
  "Returns bounds of DEF at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'def no-delimiters (interactive-p)))

(defalias 'ar-length-of-def-atpt 'ar-def-length-atpt)
(defun ar-def-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class DEF at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'def no-delimiters (interactive-p)))

(defalias 'ar-copy-def-atpt 'ar-def-copy-atpt)
(defun ar-def-copy-atpt (&optional no-delimiters)
  "Returns a copy of DEF at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'def no-delimiters (interactive-p)))

(defalias 'ar-delete-def-atpt 'ar-def-delete-atpt)
(defun ar-def-delete-atpt (&optional arg)
  "Deletes DEF at point if any. "
  (interactive "*p")
  (ar-th-delete 'def arg (interactive-p)))

(defalias 'ar-delete-def-in-region 'ar-def-delete-in-region)
(defun ar-def-delete-in-region (beg end)
  "Deletes DEF at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'def beg end (interactive-p)))

(defun ar-blok-def-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around def.
  Returns blok or nil if no DEF at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'def no-delimiters (interactive-p)))

(defun ar-doublebackslash-def-atpt (&optional no-delimiters)
  "Puts doubled backslashes around DEF at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'def no-delimiters (interactive-p)))

(defun ar-doubleslash-def-atpt (&optional no-delimiters)
  "Puts doubled slashes around DEF at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'def no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-def-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around DEF at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'def no-delimiters (interactive-p)))

(defun ar-comment-def-atpt (&optional no-delimiters)
  "Comments DEF at point if any. "
  (interactive "*p")
  (ar-th-comment 'def no-delimiters (interactive-p)))

(defun ar-commatize-def-atpt (&optional no-delimiters)
  "Put a comma after DEF at point if any. "
  (interactive "*p")
  (ar-th-commatize 'def no-delimiters (interactive-p)))

(defun ar-quote-def-atpt (&optional no-delimiters)
  "Put a singlequote before DEF at point if any. "
  (interactive "*p")
  (ar-th-quote 'def no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-def-atpt 'ar-def-hyphen-atpt)
;; ;; (defun ar-def-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around DEF at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'def no-delimiters (interactive-p)))

(defalias 'ar-mark-def-atpt 'ar-def-mark-atpt)
(defun ar-def-mark-atpt ()
  "Marks DEF at point if any. "
  (interactive)
  (ar-th-mark 'def))

(defalias 'ar-hide-def-atpt 'ar-def-hide-atpt)
(defun ar-def-hide-atpt ()
  "Hides DEF at point. "
  (interactive)
  (ar-th-hide 'def))

(defalias 'ar-show-def-atpt 'ar-def-show-atpt)
(defun ar-def-show-atpt ()
  "Shows hidden DEF at point. "
  (interactive)
  (ar-th-show 'def))

(defalias 'ar-hide-show-def-atpt 'ar-def-hide-show-atpt)
(defun ar-def-hide-show-atpt ()
  "Alternatively hides or shows DEF at point. "
  (interactive)
  (ar-th-hide-show 'def))

(defalias 'ar-highlight-def-atpt-mode 'ar-def-highlight-atpt-mode)

(defun ar-def-highlight-atpt-mode (&optional no-delimiters)
  "Toggles def-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'def no-delimiters (interactive-p)))

(defalias 'ar-kill-def-atpt 'ar-def-kill-atpt)
(defun ar-def-kill-atpt (&optional no-delimiters)
  "Kills DEF at point if any. "
  (interactive "*P")
  (ar-th-kill 'def no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-def-atpt 'ar-def-kill-backward-atpt)
(defun ar-def-kill-backward-atpt (&optional no-delimiters)
  "Kills DEF at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'def no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-def-atpt 'ar-def-leftrightsinglequote-atpt)
;; ;; (defun ar-def-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'def no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-def-atpt 'ar-def-parentize-atpt)
;; ;; (defun ar-def-parentize-atpt (&optional no-delimiters)
;;   "Parentizes DEF at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'def no-delimiters (interactive-p)))

(defalias 'ar-separate-def-atpt 'ar-def-separate-atpt)
(defun ar-def-separate-atpt (&optional no-delimiters)
  "Separates DEF at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'def no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-def-atpt 'ar-def-singlequote-atpt)
;; ;; (defun ar-def-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes DEF at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'def no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-def-atpt 'ar-def-triplequotedq-atpt)
(defun ar-def-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around def. "
  (interactive "*p")
  (ar-th-triplequotedq 'def no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-def-atpt 'ar-def-triplequotesq-atpt)
(defun ar-def-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around def. "
  (interactive "*p")
  (ar-th-triplequotesq 'def no-delimiters (interactive-p)))

(defun ar-underscore-def-atpt (&optional no-delimiters)
  "Put underscore char around DEF. "
  (interactive "*p")
  (ar-th-underscore 'def no-delimiters (interactive-p)))

;; (defalias 'ar-def-whitespace-atpt 'ar-whitespace-def-atpt)
;; ;; (defun ar-whitespace-def-atpt (&optional no-delimiters)
;;   "Put whitespace char around DEF. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'def nil t))

(defalias 'ar-forward-def-atpt 'ar-def-forward-atpt)
(defun ar-def-forward-atpt (&optional arg)
  "Moves forward over DEF at point if any, does nothing otherwise.
Returns end position of DEF "
  (interactive "p")
  (ar-th-forward 'def arg (interactive-p)))

(defalias 'ar-backward-def-atpt 'ar-def-backward-atpt)
(defun ar-def-backward-atpt (&optional arg)
  "Moves backward over DEF before point if any, does nothing otherwise.
Returns beginning position of DEF "
  (interactive "p")
  (ar-th-backward 'def arg (interactive-p)))

(defalias 'ar-transpose-def-atpt 'ar-def-transpose-atpt)
(defun ar-def-transpose-atpt (&optional arg)
  "Transposes DEF with DEF before point if any. "
  (interactive "*p")
  (ar-th-transpose 'def arg (interactive-p)))

(defalias 'ar-sort-def-atpt 'ar-def-sort-atpt)
(defun ar-def-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts defs in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'def reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-def-atpt 'ar-def-check-atpt)
(defun ar-def-check-atpt ()
  "Return t if a DEF at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-def-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-def-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-expression-atpt (&optional arg no-delimiters)
  "Returns expression at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'expression arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-expression-atpt 'ar-expression-bounds-atpt)
(defun ar-expression-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of expression if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'expression no-delimiters (interactive-p)))

(defun ar-expression-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position EXPRESSION at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'expression no-delimiters (interactive-p)))

(defun ar-expression-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'expression no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-expression-atpt 'ar-expression-beginning-atpt)
(defun ar-expression-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'expression no-delimiters (interactive-p)))

(defalias 'ar-end-of-expression-atpt 'ar-expression-end-atpt)
(defun ar-expression-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'expression no-delimiters (interactive-p)))

(defalias 'ar-in-expression-p-atpt 'ar-expression-in-p-atpt)
(defun ar-expression-in-p-atpt (&optional no-delimiters)
  "Returns bounds of EXPRESSION at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'expression no-delimiters (interactive-p)))

(defalias 'ar-length-of-expression-atpt 'ar-expression-length-atpt)
(defun ar-expression-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'expression no-delimiters (interactive-p)))

(defalias 'ar-copy-expression-atpt 'ar-expression-copy-atpt)
(defun ar-expression-copy-atpt (&optional no-delimiters)
  "Returns a copy of EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'expression no-delimiters (interactive-p)))

(defalias 'ar-delete-expression-atpt 'ar-expression-delete-atpt)
(defun ar-expression-delete-atpt (&optional arg)
  "Deletes EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-delete 'expression arg (interactive-p)))

(defalias 'ar-delete-expression-in-region 'ar-expression-delete-in-region)
(defun ar-expression-delete-in-region (beg end)
  "Deletes EXPRESSION at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'expression beg end (interactive-p)))

(defun ar-blok-expression-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around expression.
  Returns blok or nil if no EXPRESSION at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'expression no-delimiters (interactive-p)))

(defun ar-doublebackslash-expression-atpt (&optional no-delimiters)
  "Puts doubled backslashes around EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'expression no-delimiters (interactive-p)))

(defun ar-doubleslash-expression-atpt (&optional no-delimiters)
  "Puts doubled slashes around EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'expression no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-expression-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'expression no-delimiters (interactive-p)))

(defun ar-comment-expression-atpt (&optional no-delimiters)
  "Comments EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-comment 'expression no-delimiters (interactive-p)))

(defun ar-commatize-expression-atpt (&optional no-delimiters)
  "Put a comma after EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-commatize 'expression no-delimiters (interactive-p)))

(defun ar-quote-expression-atpt (&optional no-delimiters)
  "Put a singlequote before EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-quote 'expression no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-expression-atpt 'ar-expression-hyphen-atpt)
;; ;; (defun ar-expression-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around EXPRESSION at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'expression no-delimiters (interactive-p)))

(defalias 'ar-mark-expression-atpt 'ar-expression-mark-atpt)
(defun ar-expression-mark-atpt ()
  "Marks EXPRESSION at point if any. "
  (interactive)
  (ar-th-mark 'expression))

(defalias 'ar-hide-expression-atpt 'ar-expression-hide-atpt)
(defun ar-expression-hide-atpt ()
  "Hides EXPRESSION at point. "
  (interactive)
  (ar-th-hide 'expression))

(defalias 'ar-show-expression-atpt 'ar-expression-show-atpt)
(defun ar-expression-show-atpt ()
  "Shows hidden EXPRESSION at point. "
  (interactive)
  (ar-th-show 'expression))

(defalias 'ar-hide-show-expression-atpt 'ar-expression-hide-show-atpt)
(defun ar-expression-hide-show-atpt ()
  "Alternatively hides or shows EXPRESSION at point. "
  (interactive)
  (ar-th-hide-show 'expression))

(defalias 'ar-highlight-expression-atpt-mode 'ar-expression-highlight-atpt-mode)

(defun ar-expression-highlight-atpt-mode (&optional no-delimiters)
  "Toggles expression-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'expression no-delimiters (interactive-p)))

(defalias 'ar-kill-expression-atpt 'ar-expression-kill-atpt)
(defun ar-expression-kill-atpt (&optional no-delimiters)
  "Kills EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-kill 'expression no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-expression-atpt 'ar-expression-kill-backward-atpt)
(defun ar-expression-kill-backward-atpt (&optional no-delimiters)
  "Kills EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'expression no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-expression-atpt 'ar-expression-leftrightsinglequote-atpt)
;; ;; (defun ar-expression-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'expression no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-expression-atpt 'ar-expression-parentize-atpt)
;; ;; (defun ar-expression-parentize-atpt (&optional no-delimiters)
;;   "Parentizes EXPRESSION at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'expression no-delimiters (interactive-p)))

(defalias 'ar-separate-expression-atpt 'ar-expression-separate-atpt)
(defun ar-expression-separate-atpt (&optional no-delimiters)
  "Separates EXPRESSION at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'expression no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-expression-atpt 'ar-expression-singlequote-atpt)
;; ;; (defun ar-expression-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes EXPRESSION at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'expression no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-expression-atpt 'ar-expression-triplequotedq-atpt)
(defun ar-expression-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around expression. "
  (interactive "*p")
  (ar-th-triplequotedq 'expression no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-expression-atpt 'ar-expression-triplequotesq-atpt)
(defun ar-expression-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around expression. "
  (interactive "*p")
  (ar-th-triplequotesq 'expression no-delimiters (interactive-p)))

(defun ar-underscore-expression-atpt (&optional no-delimiters)
  "Put underscore char around EXPRESSION. "
  (interactive "*p")
  (ar-th-underscore 'expression no-delimiters (interactive-p)))

;; (defalias 'ar-expression-whitespace-atpt 'ar-whitespace-expression-atpt)
;; ;; (defun ar-whitespace-expression-atpt (&optional no-delimiters)
;;   "Put whitespace char around EXPRESSION. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'expression nil t))

(defalias 'ar-forward-expression-atpt 'ar-expression-forward-atpt)
(defun ar-expression-forward-atpt (&optional arg)
  "Moves forward over EXPRESSION at point if any, does nothing otherwise.
Returns end position of EXPRESSION "
  (interactive "p")
  (ar-th-forward 'expression arg (interactive-p)))

(defalias 'ar-backward-expression-atpt 'ar-expression-backward-atpt)
(defun ar-expression-backward-atpt (&optional arg)
  "Moves backward over EXPRESSION before point if any, does nothing otherwise.
Returns beginning position of EXPRESSION "
  (interactive "p")
  (ar-th-backward 'expression arg (interactive-p)))

(defalias 'ar-transpose-expression-atpt 'ar-expression-transpose-atpt)
(defun ar-expression-transpose-atpt (&optional arg)
  "Transposes EXPRESSION with EXPRESSION before point if any. "
  (interactive "*p")
  (ar-th-transpose 'expression arg (interactive-p)))

(defalias 'ar-sort-expression-atpt 'ar-expression-sort-atpt)
(defun ar-expression-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts expressions in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'expression reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-expression-atpt 'ar-expression-check-atpt)
(defun ar-expression-check-atpt ()
  "Return t if a EXPRESSION at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-expression-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-expression-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-partial-expression-atpt (&optional arg no-delimiters)
  "Returns partial-expression at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'partial-expression arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-partial-expression-atpt 'ar-partial-expression-bounds-atpt)
(defun ar-partial-expression-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of partial-expression if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'partial-expression no-delimiters (interactive-p)))

(defun ar-partial-expression-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position PARTIAL-EXPRESSION at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'partial-expression no-delimiters (interactive-p)))

(defun ar-partial-expression-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of PARTIAL-EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'partial-expression no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-partial-expression-atpt 'ar-partial-expression-beginning-atpt)
(defun ar-partial-expression-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class PARTIAL-EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'partial-expression no-delimiters (interactive-p)))

(defalias 'ar-end-of-partial-expression-atpt 'ar-partial-expression-end-atpt)
(defun ar-partial-expression-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class PARTIAL-EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'partial-expression no-delimiters (interactive-p)))

(defalias 'ar-in-partial-expression-p-atpt 'ar-partial-expression-in-p-atpt)
(defun ar-partial-expression-in-p-atpt (&optional no-delimiters)
  "Returns bounds of PARTIAL-EXPRESSION at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'partial-expression no-delimiters (interactive-p)))

(defalias 'ar-length-of-partial-expression-atpt 'ar-partial-expression-length-atpt)
(defun ar-partial-expression-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class PARTIAL-EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'partial-expression no-delimiters (interactive-p)))

(defalias 'ar-copy-partial-expression-atpt 'ar-partial-expression-copy-atpt)
(defun ar-partial-expression-copy-atpt (&optional no-delimiters)
  "Returns a copy of PARTIAL-EXPRESSION at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'partial-expression no-delimiters (interactive-p)))

(defalias 'ar-delete-partial-expression-atpt 'ar-partial-expression-delete-atpt)
(defun ar-partial-expression-delete-atpt (&optional arg)
  "Deletes PARTIAL-EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-delete 'partial-expression arg (interactive-p)))

(defalias 'ar-delete-partial-expression-in-region 'ar-partial-expression-delete-in-region)
(defun ar-partial-expression-delete-in-region (beg end)
  "Deletes PARTIAL-EXPRESSION at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'partial-expression beg end (interactive-p)))

(defun ar-blok-partial-expression-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around partial-expression.
  Returns blok or nil if no PARTIAL-EXPRESSION at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'partial-expression no-delimiters (interactive-p)))

(defun ar-doublebackslash-partial-expression-atpt (&optional no-delimiters)
  "Puts doubled backslashes around PARTIAL-EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'partial-expression no-delimiters (interactive-p)))

(defun ar-doubleslash-partial-expression-atpt (&optional no-delimiters)
  "Puts doubled slashes around PARTIAL-EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'partial-expression no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-partial-expression-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around PARTIAL-EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'partial-expression no-delimiters (interactive-p)))

(defun ar-comment-partial-expression-atpt (&optional no-delimiters)
  "Comments PARTIAL-EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-comment 'partial-expression no-delimiters (interactive-p)))

(defun ar-commatize-partial-expression-atpt (&optional no-delimiters)
  "Put a comma after PARTIAL-EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-commatize 'partial-expression no-delimiters (interactive-p)))

(defun ar-quote-partial-expression-atpt (&optional no-delimiters)
  "Put a singlequote before PARTIAL-EXPRESSION at point if any. "
  (interactive "*p")
  (ar-th-quote 'partial-expression no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-partial-expression-atpt 'ar-partial-expression-hyphen-atpt)
;; ;; (defun ar-partial-expression-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around PARTIAL-EXPRESSION at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'partial-expression no-delimiters (interactive-p)))

(defalias 'ar-mark-partial-expression-atpt 'ar-partial-expression-mark-atpt)
(defun ar-partial-expression-mark-atpt ()
  "Marks PARTIAL-EXPRESSION at point if any. "
  (interactive)
  (ar-th-mark 'partial-expression))

(defalias 'ar-hide-partial-expression-atpt 'ar-partial-expression-hide-atpt)
(defun ar-partial-expression-hide-atpt ()
  "Hides PARTIAL-EXPRESSION at point. "
  (interactive)
  (ar-th-hide 'partial-expression))

(defalias 'ar-show-partial-expression-atpt 'ar-partial-expression-show-atpt)
(defun ar-partial-expression-show-atpt ()
  "Shows hidden PARTIAL-EXPRESSION at point. "
  (interactive)
  (ar-th-show 'partial-expression))

(defalias 'ar-hide-show-partial-expression-atpt 'ar-partial-expression-hide-show-atpt)
(defun ar-partial-expression-hide-show-atpt ()
  "Alternatively hides or shows PARTIAL-EXPRESSION at point. "
  (interactive)
  (ar-th-hide-show 'partial-expression))

(defalias 'ar-highlight-partial-expression-atpt-mode 'ar-partial-expression-highlight-atpt-mode)

(defun ar-partial-expression-highlight-atpt-mode (&optional no-delimiters)
  "Toggles partial-expression-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'partial-expression no-delimiters (interactive-p)))

(defalias 'ar-kill-partial-expression-atpt 'ar-partial-expression-kill-atpt)
(defun ar-partial-expression-kill-atpt (&optional no-delimiters)
  "Kills PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-kill 'partial-expression no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-partial-expression-atpt 'ar-partial-expression-kill-backward-atpt)
(defun ar-partial-expression-kill-backward-atpt (&optional no-delimiters)
  "Kills PARTIAL-EXPRESSION at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'partial-expression no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-partial-expression-atpt 'ar-partial-expression-leftrightsinglequote-atpt)
;; ;; (defun ar-partial-expression-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'partial-expression no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-partial-expression-atpt 'ar-partial-expression-parentize-atpt)
;; ;; (defun ar-partial-expression-parentize-atpt (&optional no-delimiters)
;;   "Parentizes PARTIAL-EXPRESSION at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'partial-expression no-delimiters (interactive-p)))

(defalias 'ar-separate-partial-expression-atpt 'ar-partial-expression-separate-atpt)
(defun ar-partial-expression-separate-atpt (&optional no-delimiters)
  "Separates PARTIAL-EXPRESSION at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'partial-expression no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-partial-expression-atpt 'ar-partial-expression-singlequote-atpt)
;; ;; (defun ar-partial-expression-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes PARTIAL-EXPRESSION at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'partial-expression no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-partial-expression-atpt 'ar-partial-expression-triplequotedq-atpt)
(defun ar-partial-expression-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around partial-expression. "
  (interactive "*p")
  (ar-th-triplequotedq 'partial-expression no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-partial-expression-atpt 'ar-partial-expression-triplequotesq-atpt)
(defun ar-partial-expression-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around partial-expression. "
  (interactive "*p")
  (ar-th-triplequotesq 'partial-expression no-delimiters (interactive-p)))

(defun ar-underscore-partial-expression-atpt (&optional no-delimiters)
  "Put underscore char around PARTIAL-EXPRESSION. "
  (interactive "*p")
  (ar-th-underscore 'partial-expression no-delimiters (interactive-p)))

;; (defalias 'ar-partial-expression-whitespace-atpt 'ar-whitespace-partial-expression-atpt)
;; ;; (defun ar-whitespace-partial-expression-atpt (&optional no-delimiters)
;;   "Put whitespace char around PARTIAL-EXPRESSION. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'partial-expression nil t))

(defalias 'ar-forward-partial-expression-atpt 'ar-partial-expression-forward-atpt)
(defun ar-partial-expression-forward-atpt (&optional arg)
  "Moves forward over PARTIAL-EXPRESSION at point if any, does nothing otherwise.
Returns end position of PARTIAL-EXPRESSION "
  (interactive "p")
  (ar-th-forward 'partial-expression arg (interactive-p)))

(defalias 'ar-backward-partial-expression-atpt 'ar-partial-expression-backward-atpt)
(defun ar-partial-expression-backward-atpt (&optional arg)
  "Moves backward over PARTIAL-EXPRESSION before point if any, does nothing otherwise.
Returns beginning position of PARTIAL-EXPRESSION "
  (interactive "p")
  (ar-th-backward 'partial-expression arg (interactive-p)))

(defalias 'ar-transpose-partial-expression-atpt 'ar-partial-expression-transpose-atpt)
(defun ar-partial-expression-transpose-atpt (&optional arg)
  "Transposes PARTIAL-EXPRESSION with PARTIAL-EXPRESSION before point if any. "
  (interactive "*p")
  (ar-th-transpose 'partial-expression arg (interactive-p)))

(defalias 'ar-sort-partial-expression-atpt 'ar-partial-expression-sort-atpt)
(defun ar-partial-expression-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts partial-expressions in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'partial-expression reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-partial-expression-atpt 'ar-partial-expression-check-atpt)
(defun ar-partial-expression-check-atpt ()
  "Return t if a PARTIAL-EXPRESSION at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-partial-expression-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-partial-expression-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-statement-atpt (&optional arg no-delimiters)
  "Returns statement at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'statement arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-statement-atpt 'ar-statement-bounds-atpt)
(defun ar-statement-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of statement if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'statement no-delimiters (interactive-p)))

(defun ar-statement-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position STATEMENT at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'statement no-delimiters (interactive-p)))

(defun ar-statement-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of STATEMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'statement no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-statement-atpt 'ar-statement-beginning-atpt)
(defun ar-statement-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class STATEMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'statement no-delimiters (interactive-p)))

(defalias 'ar-end-of-statement-atpt 'ar-statement-end-atpt)
(defun ar-statement-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class STATEMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'statement no-delimiters (interactive-p)))

(defalias 'ar-in-statement-p-atpt 'ar-statement-in-p-atpt)
(defun ar-statement-in-p-atpt (&optional no-delimiters)
  "Returns bounds of STATEMENT at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'statement no-delimiters (interactive-p)))

(defalias 'ar-length-of-statement-atpt 'ar-statement-length-atpt)
(defun ar-statement-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class STATEMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'statement no-delimiters (interactive-p)))

(defalias 'ar-copy-statement-atpt 'ar-statement-copy-atpt)
(defun ar-statement-copy-atpt (&optional no-delimiters)
  "Returns a copy of STATEMENT at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'statement no-delimiters (interactive-p)))

(defalias 'ar-delete-statement-atpt 'ar-statement-delete-atpt)
(defun ar-statement-delete-atpt (&optional arg)
  "Deletes STATEMENT at point if any. "
  (interactive "*p")
  (ar-th-delete 'statement arg (interactive-p)))

(defalias 'ar-delete-statement-in-region 'ar-statement-delete-in-region)
(defun ar-statement-delete-in-region (beg end)
  "Deletes STATEMENT at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'statement beg end (interactive-p)))

(defun ar-blok-statement-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around statement.
  Returns blok or nil if no STATEMENT at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'statement no-delimiters (interactive-p)))

(defun ar-doublebackslash-statement-atpt (&optional no-delimiters)
  "Puts doubled backslashes around STATEMENT at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'statement no-delimiters (interactive-p)))

(defun ar-doubleslash-statement-atpt (&optional no-delimiters)
  "Puts doubled slashes around STATEMENT at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'statement no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-statement-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around STATEMENT at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'statement no-delimiters (interactive-p)))

(defun ar-comment-statement-atpt (&optional no-delimiters)
  "Comments STATEMENT at point if any. "
  (interactive "*p")
  (ar-th-comment 'statement no-delimiters (interactive-p)))

(defun ar-commatize-statement-atpt (&optional no-delimiters)
  "Put a comma after STATEMENT at point if any. "
  (interactive "*p")
  (ar-th-commatize 'statement no-delimiters (interactive-p)))

(defun ar-quote-statement-atpt (&optional no-delimiters)
  "Put a singlequote before STATEMENT at point if any. "
  (interactive "*p")
  (ar-th-quote 'statement no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-statement-atpt 'ar-statement-hyphen-atpt)
;; ;; (defun ar-statement-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around STATEMENT at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'statement no-delimiters (interactive-p)))

(defalias 'ar-mark-statement-atpt 'ar-statement-mark-atpt)
(defun ar-statement-mark-atpt ()
  "Marks STATEMENT at point if any. "
  (interactive)
  (ar-th-mark 'statement))

(defalias 'ar-hide-statement-atpt 'ar-statement-hide-atpt)
(defun ar-statement-hide-atpt ()
  "Hides STATEMENT at point. "
  (interactive)
  (ar-th-hide 'statement))

(defalias 'ar-show-statement-atpt 'ar-statement-show-atpt)
(defun ar-statement-show-atpt ()
  "Shows hidden STATEMENT at point. "
  (interactive)
  (ar-th-show 'statement))

(defalias 'ar-hide-show-statement-atpt 'ar-statement-hide-show-atpt)
(defun ar-statement-hide-show-atpt ()
  "Alternatively hides or shows STATEMENT at point. "
  (interactive)
  (ar-th-hide-show 'statement))

(defalias 'ar-highlight-statement-atpt-mode 'ar-statement-highlight-atpt-mode)

(defun ar-statement-highlight-atpt-mode (&optional no-delimiters)
  "Toggles statement-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'statement no-delimiters (interactive-p)))

(defalias 'ar-kill-statement-atpt 'ar-statement-kill-atpt)
(defun ar-statement-kill-atpt (&optional no-delimiters)
  "Kills STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-kill 'statement no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-statement-atpt 'ar-statement-kill-backward-atpt)
(defun ar-statement-kill-backward-atpt (&optional no-delimiters)
  "Kills STATEMENT at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'statement no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-statement-atpt 'ar-statement-leftrightsinglequote-atpt)
;; ;; (defun ar-statement-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'statement no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-statement-atpt 'ar-statement-parentize-atpt)
;; ;; (defun ar-statement-parentize-atpt (&optional no-delimiters)
;;   "Parentizes STATEMENT at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'statement no-delimiters (interactive-p)))

(defalias 'ar-separate-statement-atpt 'ar-statement-separate-atpt)
(defun ar-statement-separate-atpt (&optional no-delimiters)
  "Separates STATEMENT at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'statement no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-statement-atpt 'ar-statement-singlequote-atpt)
;; ;; (defun ar-statement-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes STATEMENT at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'statement no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-statement-atpt 'ar-statement-triplequotedq-atpt)
(defun ar-statement-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around statement. "
  (interactive "*p")
  (ar-th-triplequotedq 'statement no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-statement-atpt 'ar-statement-triplequotesq-atpt)
(defun ar-statement-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around statement. "
  (interactive "*p")
  (ar-th-triplequotesq 'statement no-delimiters (interactive-p)))

(defun ar-underscore-statement-atpt (&optional no-delimiters)
  "Put underscore char around STATEMENT. "
  (interactive "*p")
  (ar-th-underscore 'statement no-delimiters (interactive-p)))

;; (defalias 'ar-statement-whitespace-atpt 'ar-whitespace-statement-atpt)
;; ;; (defun ar-whitespace-statement-atpt (&optional no-delimiters)
;;   "Put whitespace char around STATEMENT. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'statement nil t))

(defalias 'ar-forward-statement-atpt 'ar-statement-forward-atpt)
(defun ar-statement-forward-atpt (&optional arg)
  "Moves forward over STATEMENT at point if any, does nothing otherwise.
Returns end position of STATEMENT "
  (interactive "p")
  (ar-th-forward 'statement arg (interactive-p)))

(defalias 'ar-backward-statement-atpt 'ar-statement-backward-atpt)
(defun ar-statement-backward-atpt (&optional arg)
  "Moves backward over STATEMENT before point if any, does nothing otherwise.
Returns beginning position of STATEMENT "
  (interactive "p")
  (ar-th-backward 'statement arg (interactive-p)))

(defalias 'ar-transpose-statement-atpt 'ar-statement-transpose-atpt)
(defun ar-statement-transpose-atpt (&optional arg)
  "Transposes STATEMENT with STATEMENT before point if any. "
  (interactive "*p")
  (ar-th-transpose 'statement arg (interactive-p)))

(defalias 'ar-sort-statement-atpt 'ar-statement-sort-atpt)
(defun ar-statement-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts statements in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'statement reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-statement-atpt 'ar-statement-check-atpt)
(defun ar-statement-check-atpt ()
  "Return t if a STATEMENT at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-statement-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-statement-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

(defun ar-string-atpt (&optional arg no-delimiters)
  "Returns string at point if any, nil otherwise. Optional NO-DELIMITERS trims THING, i.e. returns delimited objects like `brackteted', `braced' etc. without delimiters. "
  (interactive "p\nP")
  (ar-th 'string arg no-delimiters (interactive-p)))

(defalias 'ar-bounds-of-string-atpt 'ar-string-bounds-atpt)
(defun ar-string-bounds-atpt (&optional no-delimiters)
  "Returns a list, borders of string if any, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'string no-delimiters (interactive-p)))

(defun ar-string-beginning-position-atpt (&optional no-delimiters)
  "Returns a number, beginning position STRING at point if any, nil otherwise.  "
  (interactive "P")
  (ar-th-beg 'string no-delimiters (interactive-p)))

(defun ar-string-end-position-atpt (&optional no-delimiters)
  "Returns a number, end position of STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-end 'string no-delimiters (interactive-p)))

(defalias 'ar-beginning-of-string-atpt 'ar-string-beginning-atpt)
(defun ar-string-beginning-atpt (&optional no-delimiters)
  "Goto beginning of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotobeg 'string no-delimiters (interactive-p)))

(defalias 'ar-end-of-string-atpt 'ar-string-end-atpt)
(defun ar-string-end-atpt (&optional no-delimiters)
  "Goto end of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-gotoend 'string no-delimiters (interactive-p)))

(defalias 'ar-in-string-p-atpt 'ar-string-in-p-atpt)
(defun ar-string-in-p-atpt (&optional no-delimiters)
  "Returns bounds of STRING at point, a list, if inside, nil otherwise. "
  (interactive "P")
  (ar-th-bounds 'string no-delimiters (interactive-p)))

(defalias 'ar-length-of-string-atpt 'ar-string-length-atpt)
(defun ar-string-length-atpt (&optional no-delimiters)
  "Returns beginning of symbol or char-class STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-length 'string no-delimiters (interactive-p)))

(defalias 'ar-copy-string-atpt 'ar-string-copy-atpt)
(defun ar-string-copy-atpt (&optional no-delimiters)
  "Returns a copy of STRING at point if any, nil otherwise. "
  (interactive "P")
  (ar-th-copy 'string no-delimiters (interactive-p)))

(defalias 'ar-delete-string-atpt 'ar-string-delete-atpt)
(defun ar-string-delete-atpt (&optional arg)
  "Deletes STRING at point if any. "
  (interactive "*p")
  (ar-th-delete 'string arg (interactive-p)))

(defalias 'ar-delete-string-in-region 'ar-string-delete-in-region)
(defun ar-string-delete-in-region (beg end)
  "Deletes STRING at point if any. "
  (interactive "*r")
  (ar-th-delete-in-region 'string beg end (interactive-p)))

(defun ar-blok-string-atpt (&optional no-delimiters)
  "Puts `blok-startstring-atpt', `blok-endstring-atpt' around string.
  Returns blok or nil if no STRING at cursor-position. "
  (interactive "*p")
  (ar-th-blok 'string no-delimiters (interactive-p)))

(defun ar-doublebackslash-string-atpt (&optional no-delimiters)
  "Puts doubled backslashes around STRING at point if any. "
  (interactive "*p")
  (ar-th-doublebackslash 'string no-delimiters (interactive-p)))

(defun ar-doubleslash-string-atpt (&optional no-delimiters)
  "Puts doubled slashes around STRING at point if any. "
  (interactive "*p")
  (ar-th-doubleslash 'string no-delimiters (interactive-p)))

(defun ar-doublebackslashparen-string-atpt (&optional no-delimiters)
  "Provides doubleslashed parentheses around STRING at point if any. "
  (interactive "*p")
  (ar-th-doublebackslashparen 'string no-delimiters (interactive-p)))

(defun ar-comment-string-atpt (&optional no-delimiters)
  "Comments STRING at point if any. "
  (interactive "*p")
  (ar-th-comment 'string no-delimiters (interactive-p)))

(defun ar-commatize-string-atpt (&optional no-delimiters)
  "Put a comma after STRING at point if any. "
  (interactive "*p")
  (ar-th-commatize 'string no-delimiters (interactive-p)))

(defun ar-quote-string-atpt (&optional no-delimiters)
  "Put a singlequote before STRING at point if any. "
  (interactive "*p")
  (ar-th-quote 'string no-delimiters (interactive-p)))

;; (defalias 'ar-hyphen-string-atpt 'ar-string-hyphen-atpt)
;; ;; (defun ar-string-hyphen-atpt (&optional no-delimiters)
;;   "Puts hyphens around STRING at point if any. "
;;   (interactive "*p")
;;   (ar-th-hyphen 'string no-delimiters (interactive-p)))

(defalias 'ar-mark-string-atpt 'ar-string-mark-atpt)
(defun ar-string-mark-atpt ()
  "Marks STRING at point if any. "
  (interactive)
  (ar-th-mark 'string))

(defalias 'ar-hide-string-atpt 'ar-string-hide-atpt)
(defun ar-string-hide-atpt ()
  "Hides STRING at point. "
  (interactive)
  (ar-th-hide 'string))

(defalias 'ar-show-string-atpt 'ar-string-show-atpt)
(defun ar-string-show-atpt ()
  "Shows hidden STRING at point. "
  (interactive)
  (ar-th-show 'string))

(defalias 'ar-hide-show-string-atpt 'ar-string-hide-show-atpt)
(defun ar-string-hide-show-atpt ()
  "Alternatively hides or shows STRING at point. "
  (interactive)
  (ar-th-hide-show 'string))

(defalias 'ar-highlight-string-atpt-mode 'ar-string-highlight-atpt-mode)

(defun ar-string-highlight-atpt-mode (&optional no-delimiters)
  "Toggles string-highlight-atpt-mode "
  (interactive "P")
  (ar-th-highlight 'string no-delimiters (interactive-p)))

(defalias 'ar-kill-string-atpt 'ar-string-kill-atpt)
(defun ar-string-kill-atpt (&optional no-delimiters)
  "Kills STRING at point if any. "
  (interactive "*P")
  (ar-th-kill 'string no-delimiters (interactive-p)))

(defalias 'ar-kill-backward-string-atpt 'ar-string-kill-backward-atpt)
(defun ar-string-kill-backward-atpt (&optional no-delimiters)
  "Kills STRING at point if any. "
  (interactive "*P")
  (ar-th-kill-backward 'string no-delimiters (interactive-p)))

;; (defalias 'ar-leftrightsinglequote-string-atpt 'ar-string-leftrightsinglequote-atpt)
;; ;; (defun ar-string-leftrightsinglequote-atpt (&optional no-delimiters)
;;   "Singlequotes alnum at point if any. "
;;   (interactive "*p")
;;   (ar-th-leftrightsinglequote 'string no-delimiters (interactive-p)))

;; (defalias 'ar-parentize-string-atpt 'ar-string-parentize-atpt)
;; ;; (defun ar-string-parentize-atpt (&optional no-delimiters)
;;   "Parentizes STRING at point if any, does nothing otherwise"
;;   (interactive "*p")
;;   (ar-th-parentize 'string no-delimiters (interactive-p)))

(defalias 'ar-separate-string-atpt 'ar-string-separate-atpt)
(defun ar-string-separate-atpt (&optional no-delimiters)
  "Separates STRING at point if any, does nothing otherwise
inserts newlines, borders are the beginning or the end of buffer "
  (interactive "*p")
  (ar-th-separate 'string no-delimiters (interactive-p)))

;; (defalias 'ar-singlequote-string-atpt 'ar-string-singlequote-atpt)
;; ;; (defun ar-string-singlequote-atpt (&optional no-delimiters)
;;   "Singlequotes STRING at point if any. "
;;   (interactive "*p")
;;   (ar-th-singlequote 'string no-delimiters (interactive-p)))

(defalias 'ar-triplequotedq-string-atpt 'ar-string-triplequotedq-atpt)
(defun ar-string-triplequotedq-atpt (&optional no-delimiters)
  "Put triplequotes composed of doublequotes around string. "
  (interactive "*p")
  (ar-th-triplequotedq 'string no-delimiters (interactive-p)))

(defalias 'ar-triplequotesq-string-atpt 'ar-string-triplequotesq-atpt)
(defun ar-string-triplequotesq-atpt (&optional no-delimiters)
  "Put triplequotes composed of singlequotes around string. "
  (interactive "*p")
  (ar-th-triplequotesq 'string no-delimiters (interactive-p)))

(defun ar-underscore-string-atpt (&optional no-delimiters)
  "Put underscore char around STRING. "
  (interactive "*p")
  (ar-th-underscore 'string no-delimiters (interactive-p)))

;; (defalias 'ar-string-whitespace-atpt 'ar-whitespace-string-atpt)
;; ;; (defun ar-whitespace-string-atpt (&optional no-delimiters)
;;   "Put whitespace char around STRING. "
;;   (interactive "*p")
;;   (ar-th-whitespace 'string nil t))

(defalias 'ar-forward-string-atpt 'ar-string-forward-atpt)
(defun ar-string-forward-atpt (&optional arg)
  "Moves forward over STRING at point if any, does nothing otherwise.
Returns end position of STRING "
  (interactive "p")
  (ar-th-forward 'string arg (interactive-p)))

(defalias 'ar-backward-string-atpt 'ar-string-backward-atpt)
(defun ar-string-backward-atpt (&optional arg)
  "Moves backward over STRING before point if any, does nothing otherwise.
Returns beginning position of STRING "
  (interactive "p")
  (ar-th-backward 'string arg (interactive-p)))

(defalias 'ar-transpose-string-atpt 'ar-string-transpose-atpt)
(defun ar-string-transpose-atpt (&optional arg)
  "Transposes STRING with STRING before point if any. "
  (interactive "*p")
  (ar-th-transpose 'string arg (interactive-p)))

(defalias 'ar-sort-string-atpt 'ar-string-sort-atpt)
(defun ar-string-sort-atpt (reverse beg end &optional startkeyfun endkeyfun predicate)
  "Sorts strings in region, with ARG in reverse order.
STARTKEYFUN may be replaced by a function which stops at an alternative beginning.
ENDKEYFUN might be a function specifying THING's end when sorting.
With PREDICATE define a the function to compare. Defaults are `<' for numbers, otherwise `string<'.
See doku from `sort-subr', for details.
  "
  (interactive "*P\nr")
  (let ((reverse (when reverse)) startkeyfun endkeyfun predicate)
  (unless (use-region-p) (message "%s" "Region must be active!"))
  (ar-th-sort 'string reverse beg end startkeyfun endkeyfun predicate)))

(defalias 'ar-check-string-atpt 'ar-string-check-atpt)
(defun ar-string-check-atpt ()
  "Return t if a STRING at point exists, nil otherwise "
  (interactive)
  (let* ((beg (funcall (intern-soft (concat "ar-string-beginning-position-atpt"))))
        (end (funcall (intern-soft (concat "ar-string-end-position-atpt"))))
        (erg (ignore-errors (< beg end))))
    (when (interactive-p) (message "%s" erg))
   erg))

;; ar-thing-at-point-utils-delimiters-core: ar-atpt-expression-list end
(defun ar-backslash-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with backslash(s),
  otherwise copy backslash(ed) at point.
  With NO-DELIMITERS, copy backslash(ed) without delimiters.
  With negative argument kill backslash(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'backslash no-delimiters (interactive-p)))

(defun ar-backtick-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with backtick(s),
  otherwise copy backtick(ed) at point.
  With NO-DELIMITERS, copy backtick(ed) without delimiters.
  With negative argument kill backtick(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'backtick no-delimiters (interactive-p)))

(defun ar-colon-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with colon(s),
  otherwise copy colon(ed) at point.
  With NO-DELIMITERS, copy colon(ed) without delimiters.
  With negative argument kill colon(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'colon no-delimiters (interactive-p)))

(defun ar-cross-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with cross(s),
  otherwise copy cross(ed) at point.
  With NO-DELIMITERS, copy cross(ed) without delimiters.
  With negative argument kill cross(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'cross no-delimiters (interactive-p)))

(defun ar-dollar-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with dollar(s),
  otherwise copy dollar(ed) at point.
  With NO-DELIMITERS, copy dollar(ed) without delimiters.
  With negative argument kill dollar(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'dollar no-delimiters (interactive-p)))

(defun ar-doublequote-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with doublequote(s),
  otherwise copy doublequote(ed) at point.
  With NO-DELIMITERS, copy doublequote(ed) without delimiters.
  With negative argument kill doublequote(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'doublequote no-delimiters (interactive-p)))

(defun ar-equalize-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with equalize(s),
  otherwise copy equalize(ed) at point.
  With NO-DELIMITERS, copy equalize(ed) without delimiters.
  With negative argument kill equalize(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'equalize no-delimiters (interactive-p)))

(defun ar-escape-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with escape(s),
  otherwise copy escape(ed) at point.
  With NO-DELIMITERS, copy escape(ed) without delimiters.
  With negative argument kill escape(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'escape no-delimiters (interactive-p)))

(defun ar-hash-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with hash(s),
  otherwise copy hash(ed) at point.
  With NO-DELIMITERS, copy hash(ed) without delimiters.
  With negative argument kill hash(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'hash no-delimiters (interactive-p)))

(defun ar-hyphen-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with hyphen(s),
  otherwise copy hyphen(ed) at point.
  With NO-DELIMITERS, copy hyphen(ed) without delimiters.
  With negative argument kill hyphen(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'hyphen no-delimiters (interactive-p)))

(defun ar-singlequote-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with singlequote(s),
  otherwise copy singlequote(ed) at point.
  With NO-DELIMITERS, copy singlequote(ed) without delimiters.
  With negative argument kill singlequote(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'singlequote no-delimiters (interactive-p)))

(defun ar-slash-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with slash(s),
  otherwise copy slash(ed) at point.
  With NO-DELIMITERS, copy slash(ed) without delimiters.
  With negative argument kill slash(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'slash no-delimiters (interactive-p)))

(defun ar-star-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with star(s),
  otherwise copy star(ed) at point.
  With NO-DELIMITERS, copy star(ed) without delimiters.
  With negative argument kill star(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'star no-delimiters (interactive-p)))

(defun ar-tild-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with tild(s),
  otherwise copy tild(ed) at point.
  With NO-DELIMITERS, copy tild(ed) without delimiters.
  With negative argument kill tild(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'tild no-delimiters (interactive-p)))

(defun ar-underscore-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with underscore(s),
  otherwise copy underscore(ed) at point.
  With NO-DELIMITERS, copy underscore(ed) without delimiters.
  With negative argument kill underscore(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'underscore no-delimiters (interactive-p)))

(defun ar-whitespace-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with whitespace(s),
  otherwise copy whitespace(ed) at point.
  With NO-DELIMITERS, copy whitespace(ed) without delimiters.
  With negative argument kill whitespace(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'whitespace no-delimiters (interactive-p)))

(defun ar-doubleslash-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with doubleslash(s),
  otherwise copy doubleslash(ed) at point.
  With NO-DELIMITERS, copy doubleslash(ed) without delimiters.
  With negative argument kill doubleslash(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'doubleslash no-delimiters (interactive-p)))

(defun ar-brace-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with brace(s),
  otherwise copy brace(ed) at point.
  With NO-DELIMITERS, copy brace(ed) without delimiters.
  With negative argument kill brace(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'brace no-delimiters (interactive-p)))

(defun ar-bracket-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with bracket(s),
  otherwise copy bracket(ed) at point.
  With NO-DELIMITERS, copy bracket(ed) without delimiters.
  With negative argument kill bracket(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'bracket no-delimiters (interactive-p)))

(defun ar-lesserangle-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with lesserangle(s),
  otherwise copy lesserangle(ed) at point.
  With NO-DELIMITERS, copy lesserangle(ed) without delimiters.
  With negative argument kill lesserangle(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'lesserangle no-delimiters (interactive-p)))

(defun ar-greaterangle-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with greaterangle(s),
  otherwise copy greaterangle(ed) at point.
  With NO-DELIMITERS, copy greaterangle(ed) without delimiters.
  With negative argument kill greaterangle(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'greaterangle no-delimiters (interactive-p)))

(defun ar-leftrightsinglequote-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with leftrightsinglequote(s),
  otherwise copy leftrightsinglequote(ed) at point.
  With NO-DELIMITERS, copy leftrightsinglequote(ed) without delimiters.
  With negative argument kill leftrightsinglequote(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'leftrightsinglequote no-delimiters (interactive-p)))

(defun ar-leftrightsinglequote-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with leftrightsinglequote(s),
  otherwise copy leftrightsinglequote(ed) at point.
  With NO-DELIMITERS, copy leftrightsinglequote(ed) without delimiters.
  With negative argument kill leftrightsinglequote(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'leftrightsinglequote no-delimiters (interactive-p)))

(defun ar-parentize-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with parentize(s),
  otherwise copy parentize(ed) at point.
  With NO-DELIMITERS, copy parentize(ed) without delimiters.
  With negative argument kill parentize(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'parentize no-delimiters (interactive-p)))

(defun ar-greateranglednested-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with greateranglednested(s),
  otherwise copy greateranglednested(ed) at point.
  With NO-DELIMITERS, copy greateranglednested(ed) without delimiters.
  With negative argument kill greateranglednested(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'greateranglednested no-delimiters (interactive-p)))

(defun ar-lesseranglednested-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with lesseranglednested(s),
  otherwise copy lesseranglednested(ed) at point.
  With NO-DELIMITERS, copy lesseranglednested(ed) without delimiters.
  With negative argument kill lesseranglednested(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'lesseranglednested no-delimiters (interactive-p)))

(defun ar-buffer-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with buffer(s),
  otherwise copy buffer(ed) at point.
  With NO-DELIMITERS, copy buffer(ed) without delimiters.
  With negative argument kill buffer(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'buffer no-delimiters (interactive-p)))

(defun ar-comment-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with comment(s),
  otherwise copy comment(ed) at point.
  With NO-DELIMITERS, copy comment(ed) without delimiters.
  With negative argument kill comment(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'comment no-delimiters (interactive-p)))

(defun ar-csv-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with csv(s),
  otherwise copy csv(ed) at point.
  With NO-DELIMITERS, copy csv(ed) without delimiters.
  With negative argument kill csv(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'csv no-delimiters (interactive-p)))

(defun ar-date-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with date(s),
  otherwise copy date(ed) at point.
  With NO-DELIMITERS, copy date(ed) without delimiters.
  With negative argument kill date(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'date no-delimiters (interactive-p)))

(defun ar-delimited-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with delimited(s),
  otherwise copy delimited(ed) at point.
  With NO-DELIMITERS, copy delimited(ed) without delimiters.
  With negative argument kill delimited(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'delimited no-delimiters (interactive-p)))

(defun ar-email-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with email(s),
  otherwise copy email(ed) at point.
  With NO-DELIMITERS, copy email(ed) without delimiters.
  With negative argument kill email(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'email no-delimiters (interactive-p)))

(defun ar-filename-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with filename(s),
  otherwise copy filename(ed) at point.
  With NO-DELIMITERS, copy filename(ed) without delimiters.
  With negative argument kill filename(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'filename no-delimiters (interactive-p)))

(defun ar-filenamenondirectory-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with filenamenondirectory(s),
  otherwise copy filenamenondirectory(ed) at point.
  With NO-DELIMITERS, copy filenamenondirectory(ed) without delimiters.
  With negative argument kill filenamenondirectory(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'filenamenondirectory no-delimiters (interactive-p)))

(defun ar-float-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with float(s),
  otherwise copy float(ed) at point.
  With NO-DELIMITERS, copy float(ed) without delimiters.
  With negative argument kill float(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'float no-delimiters (interactive-p)))

(defun ar-function-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with function(s),
  otherwise copy function(ed) at point.
  With NO-DELIMITERS, copy function(ed) without delimiters.
  With negative argument kill function(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'function no-delimiters (interactive-p)))

(defun ar-ip-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with ip(s),
  otherwise copy ip(ed) at point.
  With NO-DELIMITERS, copy ip(ed) without delimiters.
  With negative argument kill ip(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'ip no-delimiters (interactive-p)))

(defun ar-isbn-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with isbn(s),
  otherwise copy isbn(ed) at point.
  With NO-DELIMITERS, copy isbn(ed) without delimiters.
  With negative argument kill isbn(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'isbn no-delimiters (interactive-p)))

(defun ar-line-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with line(s),
  otherwise copy line(ed) at point.
  With NO-DELIMITERS, copy line(ed) without delimiters.
  With negative argument kill line(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'line no-delimiters (interactive-p)))

(defun ar-list-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with list(s),
  otherwise copy list(ed) at point.
  With NO-DELIMITERS, copy list(ed) without delimiters.
  With negative argument kill list(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'list no-delimiters (interactive-p)))

(defun ar-name-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with name(s),
  otherwise copy name(ed) at point.
  With NO-DELIMITERS, copy name(ed) without delimiters.
  With negative argument kill name(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'name no-delimiters (interactive-p)))

(defun ar-number-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with number(s),
  otherwise copy number(ed) at point.
  With NO-DELIMITERS, copy number(ed) without delimiters.
  With negative argument kill number(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'number no-delimiters (interactive-p)))

(defun ar-page-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with page(s),
  otherwise copy page(ed) at point.
  With NO-DELIMITERS, copy page(ed) without delimiters.
  With negative argument kill page(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'page no-delimiters (interactive-p)))

(defun ar-paragraph-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with paragraph(s),
  otherwise copy paragraph(ed) at point.
  With NO-DELIMITERS, copy paragraph(ed) without delimiters.
  With negative argument kill paragraph(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'paragraph no-delimiters (interactive-p)))

(defun ar-phone-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with phone(s),
  otherwise copy phone(ed) at point.
  With NO-DELIMITERS, copy phone(ed) without delimiters.
  With negative argument kill phone(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'phone no-delimiters (interactive-p)))

(defun ar-region-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with region(s),
  otherwise copy region(ed) at point.
  With NO-DELIMITERS, copy region(ed) without delimiters.
  With negative argument kill region(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'region no-delimiters (interactive-p)))

(defun ar-sentence-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with sentence(s),
  otherwise copy sentence(ed) at point.
  With NO-DELIMITERS, copy sentence(ed) without delimiters.
  With negative argument kill sentence(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'sentence no-delimiters (interactive-p)))

(defun ar-sexp-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with sexp(s),
  otherwise copy sexp(ed) at point.
  With NO-DELIMITERS, copy sexp(ed) without delimiters.
  With negative argument kill sexp(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'sexp no-delimiters (interactive-p)))

(defun ar-string-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with string(s),
  otherwise copy string(ed) at point.
  With NO-DELIMITERS, copy string(ed) without delimiters.
  With negative argument kill string(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'string no-delimiters (interactive-p)))

(defun ar-shstruct-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with shstruct(s),
  otherwise copy shstruct(ed) at point.
  With NO-DELIMITERS, copy shstruct(ed) without delimiters.
  With negative argument kill shstruct(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'shstruct no-delimiters (interactive-p)))

(defun ar-symbol-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with symbol(s),
  otherwise copy symbol(ed) at point.
  With NO-DELIMITERS, copy symbol(ed) without delimiters.
  With negative argument kill symbol(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'symbol no-delimiters (interactive-p)))

(defun ar-url-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with url(s),
  otherwise copy url(ed) at point.
  With NO-DELIMITERS, copy url(ed) without delimiters.
  With negative argument kill url(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'url no-delimiters (interactive-p)))

(defun ar-word-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with word(s),
  otherwise copy word(ed) at point.
  With NO-DELIMITERS, copy word(ed) without delimiters.
  With negative argument kill word(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'word no-delimiters (interactive-p)))

(defun ar-wordalphaonly-or-copy-atpt (&optional no-delimiters)
  "If region is highlighted, provide THING at point with wordalphaonly(s),
  otherwise copy wordalphaonly(ed) at point.
  With NO-DELIMITERS, copy wordalphaonly(ed) without delimiters.
  With negative argument kill wordalphaonly(ed) at point. "
  (interactive "p")
  (ar-th-base-copy-or 'wordalphaonly no-delimiters (interactive-p)))



(defun emacs-batch-expression ()
  "Copy and highlight an expression starting with \"eval\" or \"load\". "
  (interactive)
  (unless (looking-back "[ \t\r\n\f]")
    (skip-chars-backward " \t\r\n\f"))

  (let ((beg (cond ((or (looking-at "--eval")(looking-at "-load"))
                    (match-beginning 0))
                   ((re-search-backward "--eval\\|-load\\|--funcall" (line-beginning-position) 'move)
                    (match-beginning 0)))))
    (if beg
        (progn
          (push-mark (point) t t)
          (setq end
                (progn
                  (skip-chars-forward "^ \t\r\n\f")
                  (skip-chars-forward " \t\r\n\f")
                  (if (looking-at "\"")
                      (progn
                        (forward-char 1)
                        (ar-end-of-doublequoted-atpt)
                        (forward-char 1)
                        (point))
                    (skip-chars-forward "^ \t\r\n\f")
                    (point))))
          (exchange-point-and-mark)
          (kill-new (buffer-substring-no-properties beg end)))
      (message "%s" "Can't detect beginning of emacs-batch-expression"))))

(defun ar-trim-list-atpt (&optional no-delimiters)
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'list t t))

(defun ar-trim-list-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'list t nil))

(defun ar-trim-list-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'list nil t))

(defun ar-trim-region-atpt ()
  "Removes leading and trailing char. "
  (interactive "*")
  (ar-th-trim 'region t t))

(defun ar-trim-region-left-atpt ()
  "Removes leading char. "
  (interactive "*")
  (ar-th-trim 'region t nil))

(defun ar-trim-region-right-atpt ()
  "Removes trailing char. "
  (interactive "*")
  (ar-th-trim 'region nil t))



(provide 'thing-at-point-utils)
;;; thing-at-point-utils.el ends here
