;;; thing-unpaired-delimited-list-in-delimited-list.el --- thing-in-thing functions
;; Copyright (C) 2010-2016 Andreas Röhler, unless
;; indicated otherwise

;; Author: Andreas Röhler <andreas.roehler@easy-emacs.de>, unless
;; indicated otherwise

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

;; Functions delivered with this THING-in-THING 
;; group of files employ several functions at a
;; defined THING within the range of a second THING.
;; 
;; `ar-hide-bracketed-in-line-atpt' for example hides everything insides brackets within the given line. Resp. `ar-show-bracketed-in-line-atpt' or a function which toggles this state: ar-hide-show-bracketed-in-line-atpt.
;; 
;; Referring which implementations are to be expected, see the
;; contents lists at the bottom of thingatpt-utils-core.el which are
;; cross-used in general.

;; Further information is given with thingatpt-utils-core.el

(defun ar-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of  at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th arg))

(defun ar-greaterangle-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-greaterangle arg))

(defun ar-lesserangle-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-lesserangle arg))

(defun ar-backslash-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-backslash arg))

(defun ar-backtick-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-backtick arg))

(defun ar-beg-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of BEG at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-beg arg))

(defun ar-blok-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of BLOK at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-blok arg))

(defun ar-bounds-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-bounds arg))

(defun ar-brace-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of BRACE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-brace arg))

(defun ar-bracket-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-bracket arg))

(defun ar-commatize-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-commatize arg))

(defun ar-comment-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-comment arg))

(defun ar-dollar-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-dollar arg))

(defun ar-double-backslash-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-double-backslash arg))

(defun ar-doublequote-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-doublequote arg))

(defun ar-doubleslash-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-double-backslash-paren arg))

(defun ar-end-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of END at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-end arg))

(defun ar-escape-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-escape arg))

(defun ar-hide-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of HIDE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-hide arg))

(defun ar-hide-show-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-hide-show arg))

(defun ar-hyphen-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-hyphen arg))

(defun ar-kill-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of KILL at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-kill arg))

(defun ar-left-right-singlequote-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-left-right-singlequote arg))

(defun ar-length-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-length arg))

(defun ar-parentize-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-parentize arg))

(defun ar-quote-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-quote arg))

(defun ar-separate-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-separate arg))

(defun ar-show-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of SHOW at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-show arg))

(defun ar-singlequote-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-singlequote arg))

(defun ar-slash-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of SLASH at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-slash arg))

(defun ar-slashparen-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-slashparen arg))

(defun ar-sort-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of SORT at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-sort arg))

(defun ar-trim-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of TRIM at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-trim arg))

(defun ar-trim-left-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-trim-left arg))

(defun ar-trim-right-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-trim-right arg))

(defun ar-underscore-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-underscore arg))

(defun ar-whitespace-backslashed-in-braced-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'braced 'ar-th-whitespace arg))

(defun ar-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of  at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th arg))

(defun ar-greaterangle-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-greaterangle arg))

(defun ar-lesserangle-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-lesserangle arg))

(defun ar-backslash-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-backslash arg))

(defun ar-backtick-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-backtick arg))

(defun ar-beg-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BEG at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-beg arg))

(defun ar-blok-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BLOK at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-blok arg))

(defun ar-bounds-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-bounds arg))

(defun ar-brace-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-brace arg))

(defun ar-bracket-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-bracket arg))

(defun ar-commatize-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-commatize arg))

(defun ar-comment-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-comment arg))

(defun ar-dollar-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-dollar arg))

(defun ar-double-backslash-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-double-backslash arg))

(defun ar-doublequote-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-doublequote arg))

(defun ar-doubleslash-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-double-backslash-paren arg))

(defun ar-end-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of END at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-end arg))

(defun ar-escape-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-escape arg))

(defun ar-hide-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-hide arg))

(defun ar-hide-show-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-hide-show arg))

(defun ar-hyphen-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-hyphen arg))

(defun ar-kill-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of KILL at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-kill arg))

(defun ar-left-right-singlequote-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-left-right-singlequote arg))

(defun ar-length-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-length arg))

(defun ar-parentize-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-parentize arg))

(defun ar-quote-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-quote arg))

(defun ar-separate-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-separate arg))

(defun ar-show-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of SHOW at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-show arg))

(defun ar-singlequote-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-singlequote arg))

(defun ar-slash-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASH at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-slash arg))

(defun ar-slashparen-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-slashparen arg))

(defun ar-sort-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of SORT at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-sort arg))

(defun ar-trim-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIM at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-trim arg))

(defun ar-trim-left-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-trim-left arg))

(defun ar-trim-right-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-trim-right arg))

(defun ar-underscore-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-underscore arg))

(defun ar-whitespace-backslashed-in-bracketed-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'bracketed 'ar-th-whitespace arg))

(defun ar-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of  at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th arg))

(defun ar-greaterangle-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-lesserangle arg))

(defun ar-backslash-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-backslash arg))

(defun ar-backtick-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-backtick arg))

(defun ar-beg-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BEG at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-beg arg))

(defun ar-blok-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-blok arg))

(defun ar-bounds-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-bounds arg))

(defun ar-brace-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-brace arg))

(defun ar-bracket-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-bracket arg))

(defun ar-commatize-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-commatize arg))

(defun ar-comment-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-comment arg))

(defun ar-dollar-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-dollar arg))

(defun ar-double-backslash-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-double-backslash arg))

(defun ar-doublequote-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-doublequote arg))

(defun ar-doubleslash-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-double-backslash-paren arg))

(defun ar-end-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of END at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-end arg))

(defun ar-escape-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-escape arg))

(defun ar-hide-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-hide arg))

(defun ar-hide-show-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-hide-show arg))

(defun ar-hyphen-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-hyphen arg))

(defun ar-kill-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of KILL at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-left-right-singlequote arg))

(defun ar-length-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-length arg))

(defun ar-parentize-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-parentize arg))

(defun ar-quote-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-quote arg))

(defun ar-separate-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-separate arg))

(defun ar-show-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-show arg))

(defun ar-singlequote-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-singlequote arg))

(defun ar-slash-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-slash arg))

(defun ar-slashparen-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-slashparen arg))

(defun ar-sort-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of SORT at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-sort arg))

(defun ar-trim-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-trim arg))

(defun ar-trim-left-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-trim-left arg))

(defun ar-trim-right-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-trim-right arg))

(defun ar-underscore-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-underscore arg))

(defun ar-whitespace-backslashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'lesserangled 'ar-th-whitespace arg))

(defun ar-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of  at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th arg))

(defun ar-greaterangle-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-lesserangle arg))

(defun ar-backslash-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-backslash arg))

(defun ar-backtick-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-backtick arg))

(defun ar-beg-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BEG at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-beg arg))

(defun ar-blok-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-blok arg))

(defun ar-bounds-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-bounds arg))

(defun ar-brace-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-brace arg))

(defun ar-bracket-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-bracket arg))

(defun ar-commatize-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-commatize arg))

(defun ar-comment-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-comment arg))

(defun ar-dollar-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-dollar arg))

(defun ar-double-backslash-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-double-backslash arg))

(defun ar-doublequote-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-doublequote arg))

(defun ar-doubleslash-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-double-backslash-paren arg))

(defun ar-end-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of END at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-end arg))

(defun ar-escape-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-escape arg))

(defun ar-hide-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-hide arg))

(defun ar-hide-show-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-hide-show arg))

(defun ar-hyphen-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-hyphen arg))

(defun ar-kill-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of KILL at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-left-right-singlequote arg))

(defun ar-length-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-length arg))

(defun ar-parentize-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-parentize arg))

(defun ar-quote-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-quote arg))

(defun ar-separate-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-separate arg))

(defun ar-show-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-show arg))

(defun ar-singlequote-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-singlequote arg))

(defun ar-slash-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-slash arg))

(defun ar-slashparen-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-slashparen arg))

(defun ar-sort-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of SORT at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-sort arg))

(defun ar-trim-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-trim arg))

(defun ar-trim-left-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-trim-left arg))

(defun ar-trim-right-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-trim-right arg))

(defun ar-underscore-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-underscore arg))

(defun ar-whitespace-backslashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'greaterangled 'ar-th-whitespace arg))

(defun ar-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of  at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th arg))

(defun ar-greaterangle-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-greaterangle arg))

(defun ar-lesserangle-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-lesserangle arg))

(defun ar-backslash-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-backslash arg))

(defun ar-backtick-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-backtick arg))

(defun ar-beg-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BEG at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-beg arg))

(defun ar-blok-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BLOK at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-blok arg))

(defun ar-bounds-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-bounds arg))

(defun ar-brace-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-brace arg))

(defun ar-bracket-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-bracket arg))

(defun ar-commatize-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-commatize arg))

(defun ar-comment-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-comment arg))

(defun ar-dollar-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-dollar arg))

(defun ar-double-backslash-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-double-backslash arg))

(defun ar-doublequote-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-doublequote arg))

(defun ar-doubleslash-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-double-backslash-paren arg))

(defun ar-end-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of END at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-end arg))

(defun ar-escape-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-escape arg))

(defun ar-hide-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-hide arg))

(defun ar-hide-show-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-hide-show arg))

(defun ar-hyphen-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-hyphen arg))

(defun ar-kill-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of KILL at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-kill arg))

(defun ar-left-right-singlequote-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-left-right-singlequote arg))

(defun ar-length-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-length arg))

(defun ar-parentize-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-parentize arg))

(defun ar-quote-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-quote arg))

(defun ar-separate-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-separate arg))

(defun ar-show-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SHOW at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-show arg))

(defun ar-singlequote-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-singlequote arg))

(defun ar-slash-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASH at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-slash arg))

(defun ar-slashparen-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-slashparen arg))

(defun ar-sort-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SORT at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-sort arg))

(defun ar-trim-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIM at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-trim arg))

(defun ar-trim-left-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-trim-left arg))

(defun ar-trim-right-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-trim-right arg))

(defun ar-underscore-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-underscore arg))

(defun ar-whitespace-backslashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'left-right-singlequoted 'ar-th-whitespace arg))

(defun ar-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of  at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th arg))

(defun ar-greaterangle-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-greaterangle arg))

(defun ar-lesserangle-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-lesserangle arg))

(defun ar-backslash-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-backslash arg))

(defun ar-backtick-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-backtick arg))

(defun ar-beg-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of BEG at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-beg arg))

(defun ar-blok-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of BLOK at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-blok arg))

(defun ar-bounds-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-bounds arg))

(defun ar-brace-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of BRACE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-brace arg))

(defun ar-bracket-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-bracket arg))

(defun ar-commatize-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-commatize arg))

(defun ar-comment-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-comment arg))

(defun ar-dollar-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-dollar arg))

(defun ar-double-backslash-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-double-backslash arg))

(defun ar-doublequote-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-doublequote arg))

(defun ar-doubleslash-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-double-backslash-paren arg))

(defun ar-end-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of END at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-end arg))

(defun ar-escape-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-escape arg))

(defun ar-hide-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of HIDE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-hide arg))

(defun ar-hide-show-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-hide-show arg))

(defun ar-hyphen-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-hyphen arg))

(defun ar-kill-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of KILL at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-kill arg))

(defun ar-left-right-singlequote-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-left-right-singlequote arg))

(defun ar-length-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-length arg))

(defun ar-parentize-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-parentize arg))

(defun ar-quote-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-quote arg))

(defun ar-separate-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-separate arg))

(defun ar-show-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of SHOW at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-show arg))

(defun ar-singlequote-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-singlequote arg))

(defun ar-slash-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of SLASH at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-slash arg))

(defun ar-slashparen-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-slashparen arg))

(defun ar-sort-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of SORT at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-sort arg))

(defun ar-trim-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of TRIM at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-trim arg))

(defun ar-trim-left-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-trim-left arg))

(defun ar-trim-right-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-trim-right arg))

(defun ar-underscore-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-underscore arg))

(defun ar-whitespace-backslashed-in-parentized-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backslashed 'parentized 'ar-th-whitespace arg))

(defun ar-backticked-in-braced-atpt (&optional arg)
  "Employ actions of  at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th arg))

(defun ar-greaterangle-backticked-in-braced-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-greaterangle arg))

(defun ar-lesserangle-backticked-in-braced-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-lesserangle arg))

(defun ar-backslash-backticked-in-braced-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-backslash arg))

(defun ar-backtick-backticked-in-braced-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-backtick arg))

(defun ar-beg-backticked-in-braced-atpt (&optional arg)
  "Employ actions of BEG at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-beg arg))

(defun ar-blok-backticked-in-braced-atpt (&optional arg)
  "Employ actions of BLOK at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-blok arg))

(defun ar-bounds-backticked-in-braced-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-bounds arg))

(defun ar-brace-backticked-in-braced-atpt (&optional arg)
  "Employ actions of BRACE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-brace arg))

(defun ar-bracket-backticked-in-braced-atpt (&optional arg)
  "Employ actions of BRACKET at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-bracket arg))

(defun ar-commatize-backticked-in-braced-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-commatize arg))

(defun ar-comment-backticked-in-braced-atpt (&optional arg)
  "Employ actions of COMMENT at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-comment arg))

(defun ar-dollar-backticked-in-braced-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-dollar arg))

(defun ar-double-backslash-backticked-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-double-backslash arg))

(defun ar-doublequote-backticked-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-doublequote arg))

(defun ar-doubleslash-backticked-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-backticked-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-double-backslash-paren arg))

(defun ar-end-backticked-in-braced-atpt (&optional arg)
  "Employ actions of END at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-end arg))

(defun ar-escape-backticked-in-braced-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-escape arg))

(defun ar-hide-backticked-in-braced-atpt (&optional arg)
  "Employ actions of HIDE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-hide arg))

(defun ar-hide-show-backticked-in-braced-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-hide-show arg))

(defun ar-hyphen-backticked-in-braced-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-hyphen arg))

(defun ar-kill-backticked-in-braced-atpt (&optional arg)
  "Employ actions of KILL at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-kill arg))

(defun ar-left-right-singlequote-backticked-in-braced-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-left-right-singlequote arg))

(defun ar-length-backticked-in-braced-atpt (&optional arg)
  "Employ actions of LENGTH at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-length arg))

(defun ar-parentize-backticked-in-braced-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-parentize arg))

(defun ar-quote-backticked-in-braced-atpt (&optional arg)
  "Employ actions of QUOTE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-quote arg))

(defun ar-separate-backticked-in-braced-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-separate arg))

(defun ar-show-backticked-in-braced-atpt (&optional arg)
  "Employ actions of SHOW at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-show arg))

(defun ar-singlequote-backticked-in-braced-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-singlequote arg))

(defun ar-slash-backticked-in-braced-atpt (&optional arg)
  "Employ actions of SLASH at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-slash arg))

(defun ar-slashparen-backticked-in-braced-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-slashparen arg))

(defun ar-sort-backticked-in-braced-atpt (&optional arg)
  "Employ actions of SORT at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-sort arg))

(defun ar-trim-backticked-in-braced-atpt (&optional arg)
  "Employ actions of TRIM at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-trim arg))

(defun ar-trim-left-backticked-in-braced-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-trim-left arg))

(defun ar-trim-right-backticked-in-braced-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-trim-right arg))

(defun ar-underscore-backticked-in-braced-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-underscore arg))

(defun ar-whitespace-backticked-in-braced-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'braced 'ar-th-whitespace arg))

(defun ar-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of  at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th arg))

(defun ar-greaterangle-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-greaterangle arg))

(defun ar-lesserangle-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-lesserangle arg))

(defun ar-backslash-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-backslash arg))

(defun ar-backtick-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-backtick arg))

(defun ar-beg-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of BEG at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-beg arg))

(defun ar-blok-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of BLOK at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-blok arg))

(defun ar-bounds-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-bounds arg))

(defun ar-brace-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-brace arg))

(defun ar-bracket-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACKET at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-bracket arg))

(defun ar-commatize-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-commatize arg))

(defun ar-comment-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMENT at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-comment arg))

(defun ar-dollar-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-dollar arg))

(defun ar-double-backslash-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-double-backslash arg))

(defun ar-doublequote-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-doublequote arg))

(defun ar-doubleslash-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-double-backslash-paren arg))

(defun ar-end-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of END at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-end arg))

(defun ar-escape-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-escape arg))

(defun ar-hide-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-hide arg))

(defun ar-hide-show-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-hide-show arg))

(defun ar-hyphen-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-hyphen arg))

(defun ar-kill-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of KILL at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-kill arg))

(defun ar-left-right-singlequote-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-left-right-singlequote arg))

(defun ar-length-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of LENGTH at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-length arg))

(defun ar-parentize-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-parentize arg))

(defun ar-quote-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of QUOTE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-quote arg))

(defun ar-separate-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-separate arg))

(defun ar-show-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of SHOW at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-show arg))

(defun ar-singlequote-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-singlequote arg))

(defun ar-slash-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASH at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-slash arg))

(defun ar-slashparen-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-slashparen arg))

(defun ar-sort-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of SORT at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-sort arg))

(defun ar-trim-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIM at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-trim arg))

(defun ar-trim-left-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-trim-left arg))

(defun ar-trim-right-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-trim-right arg))

(defun ar-underscore-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-underscore arg))

(defun ar-whitespace-backticked-in-bracketed-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'bracketed 'ar-th-whitespace arg))

(defun ar-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of  at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th arg))

(defun ar-greaterangle-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-lesserangle arg))

(defun ar-backslash-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-backslash arg))

(defun ar-backtick-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-backtick arg))

(defun ar-beg-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of BEG at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-beg arg))

(defun ar-blok-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-blok arg))

(defun ar-bounds-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-bounds arg))

(defun ar-brace-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-brace arg))

(defun ar-bracket-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-bracket arg))

(defun ar-commatize-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-commatize arg))

(defun ar-comment-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-comment arg))

(defun ar-dollar-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-dollar arg))

(defun ar-double-backslash-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-double-backslash arg))

(defun ar-doublequote-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-doublequote arg))

(defun ar-doubleslash-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-double-backslash-paren arg))

(defun ar-end-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of END at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-end arg))

(defun ar-escape-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-escape arg))

(defun ar-hide-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-hide arg))

(defun ar-hide-show-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-hide-show arg))

(defun ar-hyphen-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-hyphen arg))

(defun ar-kill-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of KILL at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-left-right-singlequote arg))

(defun ar-length-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-length arg))

(defun ar-parentize-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-parentize arg))

(defun ar-quote-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-quote arg))

(defun ar-separate-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-separate arg))

(defun ar-show-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-show arg))

(defun ar-singlequote-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-singlequote arg))

(defun ar-slash-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-slash arg))

(defun ar-slashparen-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-slashparen arg))

(defun ar-sort-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of SORT at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-sort arg))

(defun ar-trim-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-trim arg))

(defun ar-trim-left-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-trim-left arg))

(defun ar-trim-right-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-trim-right arg))

(defun ar-underscore-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-underscore arg))

(defun ar-whitespace-backticked-in-lesserangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'lesserangled 'ar-th-whitespace arg))

(defun ar-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of  at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th arg))

(defun ar-greaterangle-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-lesserangle arg))

(defun ar-backslash-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-backslash arg))

(defun ar-backtick-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-backtick arg))

(defun ar-beg-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of BEG at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-beg arg))

(defun ar-blok-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-blok arg))

(defun ar-bounds-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-bounds arg))

(defun ar-brace-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-brace arg))

(defun ar-bracket-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-bracket arg))

(defun ar-commatize-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-commatize arg))

(defun ar-comment-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-comment arg))

(defun ar-dollar-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-dollar arg))

(defun ar-double-backslash-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-double-backslash arg))

(defun ar-doublequote-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-doublequote arg))

(defun ar-doubleslash-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-double-backslash-paren arg))

(defun ar-end-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of END at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-end arg))

(defun ar-escape-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-escape arg))

(defun ar-hide-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-hide arg))

(defun ar-hide-show-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-hide-show arg))

(defun ar-hyphen-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-hyphen arg))

(defun ar-kill-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of KILL at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-left-right-singlequote arg))

(defun ar-length-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-length arg))

(defun ar-parentize-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-parentize arg))

(defun ar-quote-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-quote arg))

(defun ar-separate-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-separate arg))

(defun ar-show-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-show arg))

(defun ar-singlequote-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-singlequote arg))

(defun ar-slash-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-slash arg))

(defun ar-slashparen-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-slashparen arg))

(defun ar-sort-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of SORT at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-sort arg))

(defun ar-trim-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-trim arg))

(defun ar-trim-left-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-trim-left arg))

(defun ar-trim-right-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-trim-right arg))

(defun ar-underscore-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-underscore arg))

(defun ar-whitespace-backticked-in-greaterangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'greaterangled 'ar-th-whitespace arg))

(defun ar-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of  at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th arg))

(defun ar-greaterangle-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-greaterangle arg))

(defun ar-lesserangle-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-lesserangle arg))

(defun ar-backslash-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-backslash arg))

(defun ar-backtick-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-backtick arg))

(defun ar-beg-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BEG at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-beg arg))

(defun ar-blok-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BLOK at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-blok arg))

(defun ar-bounds-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-bounds arg))

(defun ar-brace-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-brace arg))

(defun ar-bracket-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACKET at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-bracket arg))

(defun ar-commatize-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-commatize arg))

(defun ar-comment-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMENT at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-comment arg))

(defun ar-dollar-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-dollar arg))

(defun ar-double-backslash-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-double-backslash arg))

(defun ar-doublequote-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-doublequote arg))

(defun ar-doubleslash-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-double-backslash-paren arg))

(defun ar-end-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of END at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-end arg))

(defun ar-escape-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-escape arg))

(defun ar-hide-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-hide arg))

(defun ar-hide-show-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-hide-show arg))

(defun ar-hyphen-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-hyphen arg))

(defun ar-kill-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of KILL at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-kill arg))

(defun ar-left-right-singlequote-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-left-right-singlequote arg))

(defun ar-length-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LENGTH at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-length arg))

(defun ar-parentize-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-parentize arg))

(defun ar-quote-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of QUOTE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-quote arg))

(defun ar-separate-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-separate arg))

(defun ar-show-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SHOW at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-show arg))

(defun ar-singlequote-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-singlequote arg))

(defun ar-slash-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASH at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-slash arg))

(defun ar-slashparen-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-slashparen arg))

(defun ar-sort-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SORT at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-sort arg))

(defun ar-trim-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIM at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-trim arg))

(defun ar-trim-left-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-trim-left arg))

(defun ar-trim-right-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-trim-right arg))

(defun ar-underscore-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-underscore arg))

(defun ar-whitespace-backticked-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'left-right-singlequoted 'ar-th-whitespace arg))

(defun ar-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of  at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th arg))

(defun ar-greaterangle-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-greaterangle arg))

(defun ar-lesserangle-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-lesserangle arg))

(defun ar-backslash-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-backslash arg))

(defun ar-backtick-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-backtick arg))

(defun ar-beg-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of BEG at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-beg arg))

(defun ar-blok-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of BLOK at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-blok arg))

(defun ar-bounds-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-bounds arg))

(defun ar-brace-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of BRACE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-brace arg))

(defun ar-bracket-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of BRACKET at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-bracket arg))

(defun ar-commatize-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-commatize arg))

(defun ar-comment-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of COMMENT at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-comment arg))

(defun ar-dollar-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-dollar arg))

(defun ar-double-backslash-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-double-backslash arg))

(defun ar-doublequote-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-doublequote arg))

(defun ar-doubleslash-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-double-backslash-paren arg))

(defun ar-end-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of END at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-end arg))

(defun ar-escape-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-escape arg))

(defun ar-hide-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of HIDE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-hide arg))

(defun ar-hide-show-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-hide-show arg))

(defun ar-hyphen-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-hyphen arg))

(defun ar-kill-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of KILL at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-kill arg))

(defun ar-left-right-singlequote-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-left-right-singlequote arg))

(defun ar-length-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of LENGTH at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-length arg))

(defun ar-parentize-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-parentize arg))

(defun ar-quote-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of QUOTE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-quote arg))

(defun ar-separate-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-separate arg))

(defun ar-show-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of SHOW at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-show arg))

(defun ar-singlequote-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-singlequote arg))

(defun ar-slash-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of SLASH at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-slash arg))

(defun ar-slashparen-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-slashparen arg))

(defun ar-sort-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of SORT at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-sort arg))

(defun ar-trim-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of TRIM at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-trim arg))

(defun ar-trim-left-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-trim-left arg))

(defun ar-trim-right-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-trim-right arg))

(defun ar-underscore-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-underscore arg))

(defun ar-whitespace-backticked-in-parentized-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'backticked 'parentized 'ar-th-whitespace arg))

(defun ar-dollared-in-braced-atpt (&optional arg)
  "Employ actions of  at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th arg))

(defun ar-greaterangle-dollared-in-braced-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-greaterangle arg))

(defun ar-lesserangle-dollared-in-braced-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-lesserangle arg))

(defun ar-backslash-dollared-in-braced-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-backslash arg))

(defun ar-backtick-dollared-in-braced-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-backtick arg))

(defun ar-beg-dollared-in-braced-atpt (&optional arg)
  "Employ actions of BEG at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-beg arg))

(defun ar-blok-dollared-in-braced-atpt (&optional arg)
  "Employ actions of BLOK at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-blok arg))

(defun ar-bounds-dollared-in-braced-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-bounds arg))

(defun ar-brace-dollared-in-braced-atpt (&optional arg)
  "Employ actions of BRACE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-brace arg))

(defun ar-bracket-dollared-in-braced-atpt (&optional arg)
  "Employ actions of BRACKET at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-bracket arg))

(defun ar-commatize-dollared-in-braced-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-commatize arg))

(defun ar-comment-dollared-in-braced-atpt (&optional arg)
  "Employ actions of COMMENT at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-comment arg))

(defun ar-dollar-dollared-in-braced-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-dollar arg))

(defun ar-double-backslash-dollared-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-double-backslash arg))

(defun ar-doublequote-dollared-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-doublequote arg))

(defun ar-doubleslash-dollared-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-dollared-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-double-backslash-paren arg))

(defun ar-end-dollared-in-braced-atpt (&optional arg)
  "Employ actions of END at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-end arg))

(defun ar-escape-dollared-in-braced-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-escape arg))

(defun ar-hide-dollared-in-braced-atpt (&optional arg)
  "Employ actions of HIDE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-hide arg))

(defun ar-hide-show-dollared-in-braced-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-hide-show arg))

(defun ar-hyphen-dollared-in-braced-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-hyphen arg))

(defun ar-kill-dollared-in-braced-atpt (&optional arg)
  "Employ actions of KILL at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-kill arg))

(defun ar-left-right-singlequote-dollared-in-braced-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-left-right-singlequote arg))

(defun ar-length-dollared-in-braced-atpt (&optional arg)
  "Employ actions of LENGTH at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-length arg))

(defun ar-parentize-dollared-in-braced-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-parentize arg))

(defun ar-quote-dollared-in-braced-atpt (&optional arg)
  "Employ actions of QUOTE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-quote arg))

(defun ar-separate-dollared-in-braced-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-separate arg))

(defun ar-show-dollared-in-braced-atpt (&optional arg)
  "Employ actions of SHOW at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-show arg))

(defun ar-singlequote-dollared-in-braced-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-singlequote arg))

(defun ar-slash-dollared-in-braced-atpt (&optional arg)
  "Employ actions of SLASH at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-slash arg))

(defun ar-slashparen-dollared-in-braced-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-slashparen arg))

(defun ar-sort-dollared-in-braced-atpt (&optional arg)
  "Employ actions of SORT at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-sort arg))

(defun ar-trim-dollared-in-braced-atpt (&optional arg)
  "Employ actions of TRIM at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-trim arg))

(defun ar-trim-left-dollared-in-braced-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-trim-left arg))

(defun ar-trim-right-dollared-in-braced-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-trim-right arg))

(defun ar-underscore-dollared-in-braced-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-underscore arg))

(defun ar-whitespace-dollared-in-braced-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'braced 'ar-th-whitespace arg))

(defun ar-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of  at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th arg))

(defun ar-greaterangle-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-greaterangle arg))

(defun ar-lesserangle-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-lesserangle arg))

(defun ar-backslash-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-backslash arg))

(defun ar-backtick-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-backtick arg))

(defun ar-beg-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of BEG at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-beg arg))

(defun ar-blok-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of BLOK at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-blok arg))

(defun ar-bounds-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-bounds arg))

(defun ar-brace-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-brace arg))

(defun ar-bracket-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACKET at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-bracket arg))

(defun ar-commatize-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-commatize arg))

(defun ar-comment-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMENT at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-comment arg))

(defun ar-dollar-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-dollar arg))

(defun ar-double-backslash-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-double-backslash arg))

(defun ar-doublequote-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-doublequote arg))

(defun ar-doubleslash-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-double-backslash-paren arg))

(defun ar-end-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of END at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-end arg))

(defun ar-escape-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-escape arg))

(defun ar-hide-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-hide arg))

(defun ar-hide-show-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-hide-show arg))

(defun ar-hyphen-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-hyphen arg))

(defun ar-kill-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of KILL at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-kill arg))

(defun ar-left-right-singlequote-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-left-right-singlequote arg))

(defun ar-length-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of LENGTH at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-length arg))

(defun ar-parentize-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-parentize arg))

(defun ar-quote-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of QUOTE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-quote arg))

(defun ar-separate-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-separate arg))

(defun ar-show-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of SHOW at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-show arg))

(defun ar-singlequote-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-singlequote arg))

(defun ar-slash-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASH at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-slash arg))

(defun ar-slashparen-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-slashparen arg))

(defun ar-sort-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of SORT at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-sort arg))

(defun ar-trim-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIM at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-trim arg))

(defun ar-trim-left-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-trim-left arg))

(defun ar-trim-right-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-trim-right arg))

(defun ar-underscore-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-underscore arg))

(defun ar-whitespace-dollared-in-bracketed-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'bracketed 'ar-th-whitespace arg))

(defun ar-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of  at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th arg))

(defun ar-greaterangle-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-lesserangle arg))

(defun ar-backslash-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-backslash arg))

(defun ar-backtick-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-backtick arg))

(defun ar-beg-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of BEG at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-beg arg))

(defun ar-blok-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-blok arg))

(defun ar-bounds-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-bounds arg))

(defun ar-brace-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-brace arg))

(defun ar-bracket-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-bracket arg))

(defun ar-commatize-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-commatize arg))

(defun ar-comment-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-comment arg))

(defun ar-dollar-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-dollar arg))

(defun ar-double-backslash-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-double-backslash arg))

(defun ar-doublequote-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-doublequote arg))

(defun ar-doubleslash-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-double-backslash-paren arg))

(defun ar-end-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of END at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-end arg))

(defun ar-escape-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-escape arg))

(defun ar-hide-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-hide arg))

(defun ar-hide-show-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-hide-show arg))

(defun ar-hyphen-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-hyphen arg))

(defun ar-kill-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of KILL at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-left-right-singlequote arg))

(defun ar-length-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-length arg))

(defun ar-parentize-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-parentize arg))

(defun ar-quote-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-quote arg))

(defun ar-separate-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-separate arg))

(defun ar-show-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-show arg))

(defun ar-singlequote-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-singlequote arg))

(defun ar-slash-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-slash arg))

(defun ar-slashparen-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-slashparen arg))

(defun ar-sort-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of SORT at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-sort arg))

(defun ar-trim-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-trim arg))

(defun ar-trim-left-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-trim-left arg))

(defun ar-trim-right-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-trim-right arg))

(defun ar-underscore-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-underscore arg))

(defun ar-whitespace-dollared-in-lesserangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'lesserangled 'ar-th-whitespace arg))

(defun ar-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of  at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th arg))

(defun ar-greaterangle-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-lesserangle arg))

(defun ar-backslash-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-backslash arg))

(defun ar-backtick-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-backtick arg))

(defun ar-beg-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of BEG at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-beg arg))

(defun ar-blok-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-blok arg))

(defun ar-bounds-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-bounds arg))

(defun ar-brace-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-brace arg))

(defun ar-bracket-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-bracket arg))

(defun ar-commatize-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-commatize arg))

(defun ar-comment-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-comment arg))

(defun ar-dollar-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-dollar arg))

(defun ar-double-backslash-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-double-backslash arg))

(defun ar-doublequote-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-doublequote arg))

(defun ar-doubleslash-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-double-backslash-paren arg))

(defun ar-end-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of END at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-end arg))

(defun ar-escape-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-escape arg))

(defun ar-hide-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-hide arg))

(defun ar-hide-show-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-hide-show arg))

(defun ar-hyphen-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-hyphen arg))

(defun ar-kill-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of KILL at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-left-right-singlequote arg))

(defun ar-length-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-length arg))

(defun ar-parentize-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-parentize arg))

(defun ar-quote-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-quote arg))

(defun ar-separate-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-separate arg))

(defun ar-show-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-show arg))

(defun ar-singlequote-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-singlequote arg))

(defun ar-slash-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-slash arg))

(defun ar-slashparen-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-slashparen arg))

(defun ar-sort-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of SORT at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-sort arg))

(defun ar-trim-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-trim arg))

(defun ar-trim-left-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-trim-left arg))

(defun ar-trim-right-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-trim-right arg))

(defun ar-underscore-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-underscore arg))

(defun ar-whitespace-dollared-in-greaterangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'greaterangled 'ar-th-whitespace arg))

(defun ar-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of  at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th arg))

(defun ar-greaterangle-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-greaterangle arg))

(defun ar-lesserangle-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-lesserangle arg))

(defun ar-backslash-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-backslash arg))

(defun ar-backtick-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-backtick arg))

(defun ar-beg-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BEG at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-beg arg))

(defun ar-blok-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BLOK at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-blok arg))

(defun ar-bounds-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-bounds arg))

(defun ar-brace-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-brace arg))

(defun ar-bracket-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACKET at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-bracket arg))

(defun ar-commatize-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-commatize arg))

(defun ar-comment-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMENT at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-comment arg))

(defun ar-dollar-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-dollar arg))

(defun ar-double-backslash-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-double-backslash arg))

(defun ar-doublequote-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-doublequote arg))

(defun ar-doubleslash-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-double-backslash-paren arg))

(defun ar-end-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of END at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-end arg))

(defun ar-escape-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-escape arg))

(defun ar-hide-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-hide arg))

(defun ar-hide-show-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-hide-show arg))

(defun ar-hyphen-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-hyphen arg))

(defun ar-kill-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of KILL at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-kill arg))

(defun ar-left-right-singlequote-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-left-right-singlequote arg))

(defun ar-length-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LENGTH at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-length arg))

(defun ar-parentize-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-parentize arg))

(defun ar-quote-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of QUOTE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-quote arg))

(defun ar-separate-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-separate arg))

(defun ar-show-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SHOW at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-show arg))

(defun ar-singlequote-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-singlequote arg))

(defun ar-slash-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASH at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-slash arg))

(defun ar-slashparen-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-slashparen arg))

(defun ar-sort-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SORT at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-sort arg))

(defun ar-trim-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIM at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-trim arg))

(defun ar-trim-left-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-trim-left arg))

(defun ar-trim-right-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-trim-right arg))

(defun ar-underscore-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-underscore arg))

(defun ar-whitespace-dollared-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'left-right-singlequoted 'ar-th-whitespace arg))

(defun ar-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of  at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th arg))

(defun ar-greaterangle-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-greaterangle arg))

(defun ar-lesserangle-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-lesserangle arg))

(defun ar-backslash-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-backslash arg))

(defun ar-backtick-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-backtick arg))

(defun ar-beg-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of BEG at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-beg arg))

(defun ar-blok-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of BLOK at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-blok arg))

(defun ar-bounds-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-bounds arg))

(defun ar-brace-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of BRACE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-brace arg))

(defun ar-bracket-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of BRACKET at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-bracket arg))

(defun ar-commatize-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-commatize arg))

(defun ar-comment-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of COMMENT at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-comment arg))

(defun ar-dollar-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-dollar arg))

(defun ar-double-backslash-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-double-backslash arg))

(defun ar-doublequote-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-doublequote arg))

(defun ar-doubleslash-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-double-backslash-paren arg))

(defun ar-end-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of END at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-end arg))

(defun ar-escape-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-escape arg))

(defun ar-hide-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of HIDE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-hide arg))

(defun ar-hide-show-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-hide-show arg))

(defun ar-hyphen-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-hyphen arg))

(defun ar-kill-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of KILL at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-kill arg))

(defun ar-left-right-singlequote-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-left-right-singlequote arg))

(defun ar-length-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of LENGTH at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-length arg))

(defun ar-parentize-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-parentize arg))

(defun ar-quote-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of QUOTE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-quote arg))

(defun ar-separate-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-separate arg))

(defun ar-show-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of SHOW at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-show arg))

(defun ar-singlequote-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-singlequote arg))

(defun ar-slash-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of SLASH at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-slash arg))

(defun ar-slashparen-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-slashparen arg))

(defun ar-sort-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of SORT at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-sort arg))

(defun ar-trim-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of TRIM at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-trim arg))

(defun ar-trim-left-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-trim-left arg))

(defun ar-trim-right-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-trim-right arg))

(defun ar-underscore-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-underscore arg))

(defun ar-whitespace-dollared-in-parentized-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'dollared 'parentized 'ar-th-whitespace arg))

(defun ar-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of  at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th arg))

(defun ar-greaterangle-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-greaterangle arg))

(defun ar-lesserangle-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-lesserangle arg))

(defun ar-backslash-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-backslash arg))

(defun ar-backtick-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-backtick arg))

(defun ar-beg-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-beg arg))

(defun ar-blok-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-blok arg))

(defun ar-bounds-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-bounds arg))

(defun ar-brace-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-brace arg))

(defun ar-bracket-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-bracket arg))

(defun ar-commatize-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-commatize arg))

(defun ar-comment-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-comment arg))

(defun ar-dollar-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-dollar arg))

(defun ar-double-backslash-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-double-backslash arg))

(defun ar-doublequote-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-doublequote arg))

(defun ar-doubleslash-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-double-backslash-paren arg))

(defun ar-end-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of END at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-end arg))

(defun ar-escape-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-escape arg))

(defun ar-hide-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-hide arg))

(defun ar-hide-show-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-hide-show arg))

(defun ar-hyphen-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-hyphen arg))

(defun ar-kill-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-kill arg))

(defun ar-left-right-singlequote-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-left-right-singlequote arg))

(defun ar-length-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-length arg))

(defun ar-parentize-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-parentize arg))

(defun ar-quote-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-quote arg))

(defun ar-separate-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-separate arg))

(defun ar-show-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-show arg))

(defun ar-singlequote-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-singlequote arg))

(defun ar-slash-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-slash arg))

(defun ar-slashparen-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-slashparen arg))

(defun ar-sort-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-sort arg))

(defun ar-trim-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-trim arg))

(defun ar-trim-left-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-trim-left arg))

(defun ar-trim-right-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-trim-right arg))

(defun ar-underscore-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-underscore arg))

(defun ar-whitespace-doublequoted-in-braced-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'braced 'ar-th-whitespace arg))

(defun ar-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of  at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th arg))

(defun ar-greaterangle-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-greaterangle arg))

(defun ar-lesserangle-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-lesserangle arg))

(defun ar-backslash-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-backslash arg))

(defun ar-backtick-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-backtick arg))

(defun ar-beg-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-beg arg))

(defun ar-blok-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-blok arg))

(defun ar-bounds-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-bounds arg))

(defun ar-brace-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-brace arg))

(defun ar-bracket-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-bracket arg))

(defun ar-commatize-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-commatize arg))

(defun ar-comment-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-comment arg))

(defun ar-dollar-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-dollar arg))

(defun ar-double-backslash-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-double-backslash arg))

(defun ar-doublequote-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-doublequote arg))

(defun ar-doubleslash-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-double-backslash-paren arg))

(defun ar-end-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of END at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-end arg))

(defun ar-escape-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-escape arg))

(defun ar-hide-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-hide arg))

(defun ar-hide-show-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-hide-show arg))

(defun ar-hyphen-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-hyphen arg))

(defun ar-kill-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-kill arg))

(defun ar-left-right-singlequote-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-left-right-singlequote arg))

(defun ar-length-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-length arg))

(defun ar-parentize-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-parentize arg))

(defun ar-quote-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-quote arg))

(defun ar-separate-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-separate arg))

(defun ar-show-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-show arg))

(defun ar-singlequote-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-singlequote arg))

(defun ar-slash-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-slash arg))

(defun ar-slashparen-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-slashparen arg))

(defun ar-sort-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-sort arg))

(defun ar-trim-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-trim arg))

(defun ar-trim-left-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-trim-left arg))

(defun ar-trim-right-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-trim-right arg))

(defun ar-underscore-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-underscore arg))

(defun ar-whitespace-doublequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'bracketed 'ar-th-whitespace arg))

(defun ar-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of  at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th arg))

(defun ar-greaterangle-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-lesserangle arg))

(defun ar-backslash-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-backslash arg))

(defun ar-backtick-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-backtick arg))

(defun ar-beg-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-beg arg))

(defun ar-blok-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-blok arg))

(defun ar-bounds-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-bounds arg))

(defun ar-brace-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-brace arg))

(defun ar-bracket-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-bracket arg))

(defun ar-commatize-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-commatize arg))

(defun ar-comment-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-comment arg))

(defun ar-dollar-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-dollar arg))

(defun ar-double-backslash-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-double-backslash arg))

(defun ar-doublequote-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-doublequote arg))

(defun ar-doubleslash-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-double-backslash-paren arg))

(defun ar-end-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of END at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-end arg))

(defun ar-escape-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-escape arg))

(defun ar-hide-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-hide arg))

(defun ar-hide-show-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-hide-show arg))

(defun ar-hyphen-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-hyphen arg))

(defun ar-kill-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-left-right-singlequote arg))

(defun ar-length-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-length arg))

(defun ar-parentize-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-parentize arg))

(defun ar-quote-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-quote arg))

(defun ar-separate-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-separate arg))

(defun ar-show-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-show arg))

(defun ar-singlequote-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-singlequote arg))

(defun ar-slash-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-slash arg))

(defun ar-slashparen-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-slashparen arg))

(defun ar-sort-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-sort arg))

(defun ar-trim-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-trim arg))

(defun ar-trim-left-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-trim-left arg))

(defun ar-trim-right-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-trim-right arg))

(defun ar-underscore-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-underscore arg))

(defun ar-whitespace-doublequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'lesserangled 'ar-th-whitespace arg))

(defun ar-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of  at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th arg))

(defun ar-greaterangle-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-lesserangle arg))

(defun ar-backslash-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-backslash arg))

(defun ar-backtick-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-backtick arg))

(defun ar-beg-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-beg arg))

(defun ar-blok-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-blok arg))

(defun ar-bounds-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-bounds arg))

(defun ar-brace-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-brace arg))

(defun ar-bracket-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-bracket arg))

(defun ar-commatize-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-commatize arg))

(defun ar-comment-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-comment arg))

(defun ar-dollar-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-dollar arg))

(defun ar-double-backslash-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-double-backslash arg))

(defun ar-doublequote-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-doublequote arg))

(defun ar-doubleslash-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-double-backslash-paren arg))

(defun ar-end-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of END at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-end arg))

(defun ar-escape-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-escape arg))

(defun ar-hide-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-hide arg))

(defun ar-hide-show-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-hide-show arg))

(defun ar-hyphen-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-hyphen arg))

(defun ar-kill-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-left-right-singlequote arg))

(defun ar-length-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-length arg))

(defun ar-parentize-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-parentize arg))

(defun ar-quote-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-quote arg))

(defun ar-separate-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-separate arg))

(defun ar-show-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-show arg))

(defun ar-singlequote-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-singlequote arg))

(defun ar-slash-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-slash arg))

(defun ar-slashparen-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-slashparen arg))

(defun ar-sort-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-sort arg))

(defun ar-trim-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-trim arg))

(defun ar-trim-left-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-trim-left arg))

(defun ar-trim-right-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-trim-right arg))

(defun ar-underscore-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-underscore arg))

(defun ar-whitespace-doublequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'greaterangled 'ar-th-whitespace arg))

(defun ar-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of  at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th arg))

(defun ar-greaterangle-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-greaterangle arg))

(defun ar-lesserangle-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-lesserangle arg))

(defun ar-backslash-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-backslash arg))

(defun ar-backtick-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-backtick arg))

(defun ar-beg-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-beg arg))

(defun ar-blok-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-blok arg))

(defun ar-bounds-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-bounds arg))

(defun ar-brace-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-brace arg))

(defun ar-bracket-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-bracket arg))

(defun ar-commatize-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-commatize arg))

(defun ar-comment-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-comment arg))

(defun ar-dollar-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-dollar arg))

(defun ar-double-backslash-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-double-backslash arg))

(defun ar-doublequote-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-doublequote arg))

(defun ar-doubleslash-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-double-backslash-paren arg))

(defun ar-end-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of END at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-end arg))

(defun ar-escape-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-escape arg))

(defun ar-hide-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-hide arg))

(defun ar-hide-show-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-hide-show arg))

(defun ar-hyphen-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-hyphen arg))

(defun ar-kill-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-kill arg))

(defun ar-left-right-singlequote-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-left-right-singlequote arg))

(defun ar-length-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-length arg))

(defun ar-parentize-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-parentize arg))

(defun ar-quote-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-quote arg))

(defun ar-separate-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-separate arg))

(defun ar-show-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-show arg))

(defun ar-singlequote-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-singlequote arg))

(defun ar-slash-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-slash arg))

(defun ar-slashparen-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-slashparen arg))

(defun ar-sort-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-sort arg))

(defun ar-trim-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-trim arg))

(defun ar-trim-left-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-trim-left arg))

(defun ar-trim-right-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-trim-right arg))

(defun ar-underscore-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-underscore arg))

(defun ar-whitespace-doublequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'left-right-singlequoted 'ar-th-whitespace arg))

(defun ar-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of  at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th arg))

(defun ar-greaterangle-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-greaterangle arg))

(defun ar-lesserangle-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-lesserangle arg))

(defun ar-backslash-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-backslash arg))

(defun ar-backtick-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-backtick arg))

(defun ar-beg-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-beg arg))

(defun ar-blok-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-blok arg))

(defun ar-bounds-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-bounds arg))

(defun ar-brace-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-brace arg))

(defun ar-bracket-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-bracket arg))

(defun ar-commatize-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-commatize arg))

(defun ar-comment-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-comment arg))

(defun ar-dollar-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-dollar arg))

(defun ar-double-backslash-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-double-backslash arg))

(defun ar-doublequote-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-doublequote arg))

(defun ar-doubleslash-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-double-backslash-paren arg))

(defun ar-end-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of END at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-end arg))

(defun ar-escape-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-escape arg))

(defun ar-hide-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-hide arg))

(defun ar-hide-show-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-hide-show arg))

(defun ar-hyphen-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-hyphen arg))

(defun ar-kill-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-kill arg))

(defun ar-left-right-singlequote-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-left-right-singlequote arg))

(defun ar-length-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-length arg))

(defun ar-parentize-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-parentize arg))

(defun ar-quote-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-quote arg))

(defun ar-separate-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-separate arg))

(defun ar-show-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-show arg))

(defun ar-singlequote-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-singlequote arg))

(defun ar-slash-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-slash arg))

(defun ar-slashparen-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-slashparen arg))

(defun ar-sort-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-sort arg))

(defun ar-trim-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-trim arg))

(defun ar-trim-left-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-trim-left arg))

(defun ar-trim-right-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-trim-right arg))

(defun ar-underscore-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-underscore arg))

(defun ar-whitespace-doublequoted-in-parentized-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'doublequoted 'parentized 'ar-th-whitespace arg))

(defun ar-equalized-in-braced-atpt (&optional arg)
  "Employ actions of  at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th arg))

(defun ar-greaterangle-equalized-in-braced-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-greaterangle arg))

(defun ar-lesserangle-equalized-in-braced-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-lesserangle arg))

(defun ar-backslash-equalized-in-braced-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-backslash arg))

(defun ar-backtick-equalized-in-braced-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-backtick arg))

(defun ar-beg-equalized-in-braced-atpt (&optional arg)
  "Employ actions of BEG at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-beg arg))

(defun ar-blok-equalized-in-braced-atpt (&optional arg)
  "Employ actions of BLOK at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-blok arg))

(defun ar-bounds-equalized-in-braced-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-bounds arg))

(defun ar-brace-equalized-in-braced-atpt (&optional arg)
  "Employ actions of BRACE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-brace arg))

(defun ar-bracket-equalized-in-braced-atpt (&optional arg)
  "Employ actions of BRACKET at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-bracket arg))

(defun ar-commatize-equalized-in-braced-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-commatize arg))

(defun ar-comment-equalized-in-braced-atpt (&optional arg)
  "Employ actions of COMMENT at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-comment arg))

(defun ar-dollar-equalized-in-braced-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-dollar arg))

(defun ar-double-backslash-equalized-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-double-backslash arg))

(defun ar-doublequote-equalized-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-doublequote arg))

(defun ar-doubleslash-equalized-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-equalized-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-double-backslash-paren arg))

(defun ar-end-equalized-in-braced-atpt (&optional arg)
  "Employ actions of END at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-end arg))

(defun ar-escape-equalized-in-braced-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-escape arg))

(defun ar-hide-equalized-in-braced-atpt (&optional arg)
  "Employ actions of HIDE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-hide arg))

(defun ar-hide-show-equalized-in-braced-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-hide-show arg))

(defun ar-hyphen-equalized-in-braced-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-hyphen arg))

(defun ar-kill-equalized-in-braced-atpt (&optional arg)
  "Employ actions of KILL at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-kill arg))

(defun ar-left-right-singlequote-equalized-in-braced-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-left-right-singlequote arg))

(defun ar-length-equalized-in-braced-atpt (&optional arg)
  "Employ actions of LENGTH at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-length arg))

(defun ar-parentize-equalized-in-braced-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-parentize arg))

(defun ar-quote-equalized-in-braced-atpt (&optional arg)
  "Employ actions of QUOTE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-quote arg))

(defun ar-separate-equalized-in-braced-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-separate arg))

(defun ar-show-equalized-in-braced-atpt (&optional arg)
  "Employ actions of SHOW at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-show arg))

(defun ar-singlequote-equalized-in-braced-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-singlequote arg))

(defun ar-slash-equalized-in-braced-atpt (&optional arg)
  "Employ actions of SLASH at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-slash arg))

(defun ar-slashparen-equalized-in-braced-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-slashparen arg))

(defun ar-sort-equalized-in-braced-atpt (&optional arg)
  "Employ actions of SORT at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-sort arg))

(defun ar-trim-equalized-in-braced-atpt (&optional arg)
  "Employ actions of TRIM at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-trim arg))

(defun ar-trim-left-equalized-in-braced-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-trim-left arg))

(defun ar-trim-right-equalized-in-braced-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-trim-right arg))

(defun ar-underscore-equalized-in-braced-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-underscore arg))

(defun ar-whitespace-equalized-in-braced-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'braced 'ar-th-whitespace arg))

(defun ar-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of  at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th arg))

(defun ar-greaterangle-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-greaterangle arg))

(defun ar-lesserangle-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-lesserangle arg))

(defun ar-backslash-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-backslash arg))

(defun ar-backtick-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-backtick arg))

(defun ar-beg-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of BEG at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-beg arg))

(defun ar-blok-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of BLOK at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-blok arg))

(defun ar-bounds-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-bounds arg))

(defun ar-brace-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-brace arg))

(defun ar-bracket-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACKET at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-bracket arg))

(defun ar-commatize-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-commatize arg))

(defun ar-comment-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMENT at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-comment arg))

(defun ar-dollar-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-dollar arg))

(defun ar-double-backslash-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-double-backslash arg))

(defun ar-doublequote-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-doublequote arg))

(defun ar-doubleslash-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-double-backslash-paren arg))

(defun ar-end-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of END at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-end arg))

(defun ar-escape-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-escape arg))

(defun ar-hide-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-hide arg))

(defun ar-hide-show-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-hide-show arg))

(defun ar-hyphen-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-hyphen arg))

(defun ar-kill-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of KILL at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-kill arg))

(defun ar-left-right-singlequote-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-left-right-singlequote arg))

(defun ar-length-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of LENGTH at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-length arg))

(defun ar-parentize-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-parentize arg))

(defun ar-quote-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of QUOTE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-quote arg))

(defun ar-separate-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-separate arg))

(defun ar-show-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of SHOW at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-show arg))

(defun ar-singlequote-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-singlequote arg))

(defun ar-slash-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASH at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-slash arg))

(defun ar-slashparen-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-slashparen arg))

(defun ar-sort-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of SORT at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-sort arg))

(defun ar-trim-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIM at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-trim arg))

(defun ar-trim-left-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-trim-left arg))

(defun ar-trim-right-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-trim-right arg))

(defun ar-underscore-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-underscore arg))

(defun ar-whitespace-equalized-in-bracketed-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'bracketed 'ar-th-whitespace arg))

(defun ar-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of  at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th arg))

(defun ar-greaterangle-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-lesserangle arg))

(defun ar-backslash-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-backslash arg))

(defun ar-backtick-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-backtick arg))

(defun ar-beg-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of BEG at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-beg arg))

(defun ar-blok-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-blok arg))

(defun ar-bounds-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-bounds arg))

(defun ar-brace-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-brace arg))

(defun ar-bracket-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-bracket arg))

(defun ar-commatize-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-commatize arg))

(defun ar-comment-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-comment arg))

(defun ar-dollar-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-dollar arg))

(defun ar-double-backslash-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-double-backslash arg))

(defun ar-doublequote-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-doublequote arg))

(defun ar-doubleslash-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-double-backslash-paren arg))

(defun ar-end-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of END at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-end arg))

(defun ar-escape-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-escape arg))

(defun ar-hide-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-hide arg))

(defun ar-hide-show-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-hide-show arg))

(defun ar-hyphen-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-hyphen arg))

(defun ar-kill-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of KILL at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-left-right-singlequote arg))

(defun ar-length-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-length arg))

(defun ar-parentize-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-parentize arg))

(defun ar-quote-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-quote arg))

(defun ar-separate-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-separate arg))

(defun ar-show-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-show arg))

(defun ar-singlequote-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-singlequote arg))

(defun ar-slash-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-slash arg))

(defun ar-slashparen-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-slashparen arg))

(defun ar-sort-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of SORT at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-sort arg))

(defun ar-trim-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-trim arg))

(defun ar-trim-left-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-trim-left arg))

(defun ar-trim-right-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-trim-right arg))

(defun ar-underscore-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-underscore arg))

(defun ar-whitespace-equalized-in-lesserangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'lesserangled 'ar-th-whitespace arg))

(defun ar-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of  at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th arg))

(defun ar-greaterangle-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-lesserangle arg))

(defun ar-backslash-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-backslash arg))

(defun ar-backtick-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-backtick arg))

(defun ar-beg-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of BEG at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-beg arg))

(defun ar-blok-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-blok arg))

(defun ar-bounds-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-bounds arg))

(defun ar-brace-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-brace arg))

(defun ar-bracket-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-bracket arg))

(defun ar-commatize-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-commatize arg))

(defun ar-comment-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-comment arg))

(defun ar-dollar-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-dollar arg))

(defun ar-double-backslash-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-double-backslash arg))

(defun ar-doublequote-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-doublequote arg))

(defun ar-doubleslash-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-double-backslash-paren arg))

(defun ar-end-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of END at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-end arg))

(defun ar-escape-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-escape arg))

(defun ar-hide-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-hide arg))

(defun ar-hide-show-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-hide-show arg))

(defun ar-hyphen-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-hyphen arg))

(defun ar-kill-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of KILL at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-left-right-singlequote arg))

(defun ar-length-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-length arg))

(defun ar-parentize-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-parentize arg))

(defun ar-quote-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-quote arg))

(defun ar-separate-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-separate arg))

(defun ar-show-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-show arg))

(defun ar-singlequote-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-singlequote arg))

(defun ar-slash-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-slash arg))

(defun ar-slashparen-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-slashparen arg))

(defun ar-sort-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of SORT at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-sort arg))

(defun ar-trim-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-trim arg))

(defun ar-trim-left-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-trim-left arg))

(defun ar-trim-right-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-trim-right arg))

(defun ar-underscore-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-underscore arg))

(defun ar-whitespace-equalized-in-greaterangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'greaterangled 'ar-th-whitespace arg))

(defun ar-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of  at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th arg))

(defun ar-greaterangle-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-greaterangle arg))

(defun ar-lesserangle-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-lesserangle arg))

(defun ar-backslash-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-backslash arg))

(defun ar-backtick-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-backtick arg))

(defun ar-beg-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BEG at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-beg arg))

(defun ar-blok-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BLOK at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-blok arg))

(defun ar-bounds-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-bounds arg))

(defun ar-brace-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-brace arg))

(defun ar-bracket-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACKET at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-bracket arg))

(defun ar-commatize-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-commatize arg))

(defun ar-comment-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMENT at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-comment arg))

(defun ar-dollar-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-dollar arg))

(defun ar-double-backslash-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-double-backslash arg))

(defun ar-doublequote-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-doublequote arg))

(defun ar-doubleslash-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-double-backslash-paren arg))

(defun ar-end-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of END at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-end arg))

(defun ar-escape-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-escape arg))

(defun ar-hide-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-hide arg))

(defun ar-hide-show-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-hide-show arg))

(defun ar-hyphen-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-hyphen arg))

(defun ar-kill-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of KILL at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-kill arg))

(defun ar-left-right-singlequote-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-left-right-singlequote arg))

(defun ar-length-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LENGTH at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-length arg))

(defun ar-parentize-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-parentize arg))

(defun ar-quote-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of QUOTE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-quote arg))

(defun ar-separate-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-separate arg))

(defun ar-show-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SHOW at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-show arg))

(defun ar-singlequote-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-singlequote arg))

(defun ar-slash-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASH at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-slash arg))

(defun ar-slashparen-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-slashparen arg))

(defun ar-sort-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SORT at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-sort arg))

(defun ar-trim-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIM at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-trim arg))

(defun ar-trim-left-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-trim-left arg))

(defun ar-trim-right-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-trim-right arg))

(defun ar-underscore-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-underscore arg))

(defun ar-whitespace-equalized-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'left-right-singlequoted 'ar-th-whitespace arg))

(defun ar-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of  at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th arg))

(defun ar-greaterangle-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-greaterangle arg))

(defun ar-lesserangle-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-lesserangle arg))

(defun ar-backslash-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-backslash arg))

(defun ar-backtick-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-backtick arg))

(defun ar-beg-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of BEG at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-beg arg))

(defun ar-blok-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of BLOK at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-blok arg))

(defun ar-bounds-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-bounds arg))

(defun ar-brace-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of BRACE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-brace arg))

(defun ar-bracket-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of BRACKET at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-bracket arg))

(defun ar-commatize-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-commatize arg))

(defun ar-comment-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of COMMENT at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-comment arg))

(defun ar-dollar-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-dollar arg))

(defun ar-double-backslash-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-double-backslash arg))

(defun ar-doublequote-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-doublequote arg))

(defun ar-doubleslash-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-double-backslash-paren arg))

(defun ar-end-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of END at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-end arg))

(defun ar-escape-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-escape arg))

(defun ar-hide-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of HIDE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-hide arg))

(defun ar-hide-show-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-hide-show arg))

(defun ar-hyphen-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-hyphen arg))

(defun ar-kill-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of KILL at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-kill arg))

(defun ar-left-right-singlequote-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-left-right-singlequote arg))

(defun ar-length-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of LENGTH at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-length arg))

(defun ar-parentize-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-parentize arg))

(defun ar-quote-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of QUOTE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-quote arg))

(defun ar-separate-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-separate arg))

(defun ar-show-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of SHOW at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-show arg))

(defun ar-singlequote-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-singlequote arg))

(defun ar-slash-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of SLASH at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-slash arg))

(defun ar-slashparen-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-slashparen arg))

(defun ar-sort-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of SORT at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-sort arg))

(defun ar-trim-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of TRIM at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-trim arg))

(defun ar-trim-left-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-trim-left arg))

(defun ar-trim-right-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-trim-right arg))

(defun ar-underscore-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-underscore arg))

(defun ar-whitespace-equalized-in-parentized-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'equalized 'parentized 'ar-th-whitespace arg))

(defun ar-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of  at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th arg))

(defun ar-greaterangle-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-greaterangle arg))

(defun ar-lesserangle-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-lesserangle arg))

(defun ar-backslash-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-backslash arg))

(defun ar-backtick-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-backtick arg))

(defun ar-beg-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of BEG at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-beg arg))

(defun ar-blok-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of BLOK at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-blok arg))

(defun ar-bounds-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-bounds arg))

(defun ar-brace-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of BRACE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-brace arg))

(defun ar-bracket-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of BRACKET at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-bracket arg))

(defun ar-commatize-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-commatize arg))

(defun ar-comment-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of COMMENT at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-comment arg))

(defun ar-dollar-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-dollar arg))

(defun ar-double-backslash-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-double-backslash arg))

(defun ar-doublequote-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-doublequote arg))

(defun ar-doubleslash-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-double-backslash-paren arg))

(defun ar-end-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of END at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-end arg))

(defun ar-escape-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-escape arg))

(defun ar-hide-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of HIDE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-hide arg))

(defun ar-hide-show-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-hide-show arg))

(defun ar-hyphen-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-hyphen arg))

(defun ar-kill-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of KILL at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-kill arg))

(defun ar-left-right-singlequote-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-left-right-singlequote arg))

(defun ar-length-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of LENGTH at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-length arg))

(defun ar-parentize-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-parentize arg))

(defun ar-quote-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of QUOTE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-quote arg))

(defun ar-separate-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-separate arg))

(defun ar-show-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of SHOW at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-show arg))

(defun ar-singlequote-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-singlequote arg))

(defun ar-slash-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of SLASH at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-slash arg))

(defun ar-slashparen-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-slashparen arg))

(defun ar-sort-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of SORT at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-sort arg))

(defun ar-trim-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of TRIM at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-trim arg))

(defun ar-trim-left-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-trim-left arg))

(defun ar-trim-right-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-trim-right arg))

(defun ar-underscore-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-underscore arg))

(defun ar-whitespace-hyphened-in-braced-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'braced 'ar-th-whitespace arg))

(defun ar-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of  at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th arg))

(defun ar-greaterangle-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-greaterangle arg))

(defun ar-lesserangle-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-lesserangle arg))

(defun ar-backslash-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-backslash arg))

(defun ar-backtick-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-backtick arg))

(defun ar-beg-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of BEG at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-beg arg))

(defun ar-blok-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of BLOK at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-blok arg))

(defun ar-bounds-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-bounds arg))

(defun ar-brace-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-brace arg))

(defun ar-bracket-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACKET at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-bracket arg))

(defun ar-commatize-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-commatize arg))

(defun ar-comment-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMENT at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-comment arg))

(defun ar-dollar-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-dollar arg))

(defun ar-double-backslash-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-double-backslash arg))

(defun ar-doublequote-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-doublequote arg))

(defun ar-doubleslash-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-double-backslash-paren arg))

(defun ar-end-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of END at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-end arg))

(defun ar-escape-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-escape arg))

(defun ar-hide-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-hide arg))

(defun ar-hide-show-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-hide-show arg))

(defun ar-hyphen-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-hyphen arg))

(defun ar-kill-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of KILL at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-kill arg))

(defun ar-left-right-singlequote-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-left-right-singlequote arg))

(defun ar-length-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of LENGTH at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-length arg))

(defun ar-parentize-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-parentize arg))

(defun ar-quote-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of QUOTE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-quote arg))

(defun ar-separate-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-separate arg))

(defun ar-show-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of SHOW at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-show arg))

(defun ar-singlequote-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-singlequote arg))

(defun ar-slash-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASH at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-slash arg))

(defun ar-slashparen-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-slashparen arg))

(defun ar-sort-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of SORT at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-sort arg))

(defun ar-trim-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIM at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-trim arg))

(defun ar-trim-left-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-trim-left arg))

(defun ar-trim-right-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-trim-right arg))

(defun ar-underscore-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-underscore arg))

(defun ar-whitespace-hyphened-in-bracketed-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'bracketed 'ar-th-whitespace arg))

(defun ar-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of  at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th arg))

(defun ar-greaterangle-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-lesserangle arg))

(defun ar-backslash-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-backslash arg))

(defun ar-backtick-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-backtick arg))

(defun ar-beg-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of BEG at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-beg arg))

(defun ar-blok-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-blok arg))

(defun ar-bounds-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-bounds arg))

(defun ar-brace-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-brace arg))

(defun ar-bracket-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-bracket arg))

(defun ar-commatize-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-commatize arg))

(defun ar-comment-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-comment arg))

(defun ar-dollar-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-dollar arg))

(defun ar-double-backslash-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-double-backslash arg))

(defun ar-doublequote-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-doublequote arg))

(defun ar-doubleslash-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-double-backslash-paren arg))

(defun ar-end-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of END at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-end arg))

(defun ar-escape-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-escape arg))

(defun ar-hide-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-hide arg))

(defun ar-hide-show-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-hide-show arg))

(defun ar-hyphen-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-hyphen arg))

(defun ar-kill-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of KILL at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-left-right-singlequote arg))

(defun ar-length-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-length arg))

(defun ar-parentize-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-parentize arg))

(defun ar-quote-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-quote arg))

(defun ar-separate-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-separate arg))

(defun ar-show-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-show arg))

(defun ar-singlequote-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-singlequote arg))

(defun ar-slash-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-slash arg))

(defun ar-slashparen-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-slashparen arg))

(defun ar-sort-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of SORT at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-sort arg))

(defun ar-trim-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-trim arg))

(defun ar-trim-left-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-trim-left arg))

(defun ar-trim-right-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-trim-right arg))

(defun ar-underscore-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-underscore arg))

(defun ar-whitespace-hyphened-in-lesserangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'lesserangled 'ar-th-whitespace arg))

(defun ar-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of  at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th arg))

(defun ar-greaterangle-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-lesserangle arg))

(defun ar-backslash-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-backslash arg))

(defun ar-backtick-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-backtick arg))

(defun ar-beg-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of BEG at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-beg arg))

(defun ar-blok-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-blok arg))

(defun ar-bounds-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-bounds arg))

(defun ar-brace-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-brace arg))

(defun ar-bracket-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-bracket arg))

(defun ar-commatize-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-commatize arg))

(defun ar-comment-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-comment arg))

(defun ar-dollar-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-dollar arg))

(defun ar-double-backslash-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-double-backslash arg))

(defun ar-doublequote-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-doublequote arg))

(defun ar-doubleslash-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-double-backslash-paren arg))

(defun ar-end-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of END at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-end arg))

(defun ar-escape-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-escape arg))

(defun ar-hide-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-hide arg))

(defun ar-hide-show-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-hide-show arg))

(defun ar-hyphen-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-hyphen arg))

(defun ar-kill-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of KILL at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-left-right-singlequote arg))

(defun ar-length-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-length arg))

(defun ar-parentize-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-parentize arg))

(defun ar-quote-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-quote arg))

(defun ar-separate-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-separate arg))

(defun ar-show-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-show arg))

(defun ar-singlequote-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-singlequote arg))

(defun ar-slash-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-slash arg))

(defun ar-slashparen-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-slashparen arg))

(defun ar-sort-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of SORT at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-sort arg))

(defun ar-trim-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-trim arg))

(defun ar-trim-left-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-trim-left arg))

(defun ar-trim-right-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-trim-right arg))

(defun ar-underscore-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-underscore arg))

(defun ar-whitespace-hyphened-in-greaterangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'greaterangled 'ar-th-whitespace arg))

(defun ar-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of  at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th arg))

(defun ar-greaterangle-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-greaterangle arg))

(defun ar-lesserangle-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-lesserangle arg))

(defun ar-backslash-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-backslash arg))

(defun ar-backtick-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-backtick arg))

(defun ar-beg-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BEG at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-beg arg))

(defun ar-blok-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BLOK at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-blok arg))

(defun ar-bounds-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-bounds arg))

(defun ar-brace-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-brace arg))

(defun ar-bracket-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACKET at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-bracket arg))

(defun ar-commatize-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-commatize arg))

(defun ar-comment-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMENT at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-comment arg))

(defun ar-dollar-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-dollar arg))

(defun ar-double-backslash-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-double-backslash arg))

(defun ar-doublequote-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-doublequote arg))

(defun ar-doubleslash-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-double-backslash-paren arg))

(defun ar-end-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of END at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-end arg))

(defun ar-escape-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-escape arg))

(defun ar-hide-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-hide arg))

(defun ar-hide-show-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-hide-show arg))

(defun ar-hyphen-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-hyphen arg))

(defun ar-kill-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of KILL at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-kill arg))

(defun ar-left-right-singlequote-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-left-right-singlequote arg))

(defun ar-length-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LENGTH at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-length arg))

(defun ar-parentize-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-parentize arg))

(defun ar-quote-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of QUOTE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-quote arg))

(defun ar-separate-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-separate arg))

(defun ar-show-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SHOW at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-show arg))

(defun ar-singlequote-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-singlequote arg))

(defun ar-slash-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASH at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-slash arg))

(defun ar-slashparen-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-slashparen arg))

(defun ar-sort-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SORT at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-sort arg))

(defun ar-trim-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIM at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-trim arg))

(defun ar-trim-left-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-trim-left arg))

(defun ar-trim-right-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-trim-right arg))

(defun ar-underscore-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-underscore arg))

(defun ar-whitespace-hyphened-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'left-right-singlequoted 'ar-th-whitespace arg))

(defun ar-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of  at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th arg))

(defun ar-greaterangle-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-greaterangle arg))

(defun ar-lesserangle-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-lesserangle arg))

(defun ar-backslash-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-backslash arg))

(defun ar-backtick-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-backtick arg))

(defun ar-beg-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of BEG at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-beg arg))

(defun ar-blok-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of BLOK at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-blok arg))

(defun ar-bounds-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-bounds arg))

(defun ar-brace-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of BRACE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-brace arg))

(defun ar-bracket-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of BRACKET at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-bracket arg))

(defun ar-commatize-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-commatize arg))

(defun ar-comment-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of COMMENT at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-comment arg))

(defun ar-dollar-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-dollar arg))

(defun ar-double-backslash-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-double-backslash arg))

(defun ar-doublequote-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-doublequote arg))

(defun ar-doubleslash-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-double-backslash-paren arg))

(defun ar-end-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of END at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-end arg))

(defun ar-escape-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-escape arg))

(defun ar-hide-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of HIDE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-hide arg))

(defun ar-hide-show-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-hide-show arg))

(defun ar-hyphen-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-hyphen arg))

(defun ar-kill-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of KILL at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-kill arg))

(defun ar-left-right-singlequote-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-left-right-singlequote arg))

(defun ar-length-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of LENGTH at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-length arg))

(defun ar-parentize-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-parentize arg))

(defun ar-quote-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of QUOTE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-quote arg))

(defun ar-separate-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-separate arg))

(defun ar-show-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of SHOW at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-show arg))

(defun ar-singlequote-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-singlequote arg))

(defun ar-slash-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of SLASH at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-slash arg))

(defun ar-slashparen-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-slashparen arg))

(defun ar-sort-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of SORT at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-sort arg))

(defun ar-trim-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of TRIM at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-trim arg))

(defun ar-trim-left-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-trim-left arg))

(defun ar-trim-right-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-trim-right arg))

(defun ar-underscore-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-underscore arg))

(defun ar-whitespace-hyphened-in-parentized-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'hyphened 'parentized 'ar-th-whitespace arg))

(defun ar-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of  at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th arg))

(defun ar-greaterangle-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-greaterangle arg))

(defun ar-lesserangle-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-lesserangle arg))

(defun ar-backslash-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-backslash arg))

(defun ar-backtick-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-backtick arg))

(defun ar-beg-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-beg arg))

(defun ar-blok-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-blok arg))

(defun ar-bounds-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-bounds arg))

(defun ar-brace-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-brace arg))

(defun ar-bracket-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-bracket arg))

(defun ar-commatize-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-commatize arg))

(defun ar-comment-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-comment arg))

(defun ar-dollar-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-dollar arg))

(defun ar-double-backslash-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-double-backslash arg))

(defun ar-doublequote-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-doublequote arg))

(defun ar-doubleslash-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-double-backslash-paren arg))

(defun ar-end-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of END at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-end arg))

(defun ar-escape-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-escape arg))

(defun ar-hide-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-hide arg))

(defun ar-hide-show-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-hide-show arg))

(defun ar-hyphen-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-hyphen arg))

(defun ar-kill-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-kill arg))

(defun ar-left-right-singlequote-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-left-right-singlequote arg))

(defun ar-length-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-length arg))

(defun ar-parentize-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-parentize arg))

(defun ar-quote-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-quote arg))

(defun ar-separate-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-separate arg))

(defun ar-show-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-show arg))

(defun ar-singlequote-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-singlequote arg))

(defun ar-slash-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-slash arg))

(defun ar-slashparen-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-slashparen arg))

(defun ar-sort-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-sort arg))

(defun ar-trim-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-trim arg))

(defun ar-trim-left-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-trim-left arg))

(defun ar-trim-right-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-trim-right arg))

(defun ar-underscore-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-underscore arg))

(defun ar-whitespace-singlequoted-in-braced-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'braced 'ar-th-whitespace arg))

(defun ar-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of  at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th arg))

(defun ar-greaterangle-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-greaterangle arg))

(defun ar-lesserangle-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-lesserangle arg))

(defun ar-backslash-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-backslash arg))

(defun ar-backtick-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-backtick arg))

(defun ar-beg-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-beg arg))

(defun ar-blok-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-blok arg))

(defun ar-bounds-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-bounds arg))

(defun ar-brace-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-brace arg))

(defun ar-bracket-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-bracket arg))

(defun ar-commatize-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-commatize arg))

(defun ar-comment-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-comment arg))

(defun ar-dollar-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-dollar arg))

(defun ar-double-backslash-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-double-backslash arg))

(defun ar-doublequote-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-doublequote arg))

(defun ar-doubleslash-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-double-backslash-paren arg))

(defun ar-end-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of END at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-end arg))

(defun ar-escape-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-escape arg))

(defun ar-hide-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-hide arg))

(defun ar-hide-show-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-hide-show arg))

(defun ar-hyphen-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-hyphen arg))

(defun ar-kill-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-kill arg))

(defun ar-left-right-singlequote-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-left-right-singlequote arg))

(defun ar-length-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-length arg))

(defun ar-parentize-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-parentize arg))

(defun ar-quote-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-quote arg))

(defun ar-separate-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-separate arg))

(defun ar-show-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-show arg))

(defun ar-singlequote-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-singlequote arg))

(defun ar-slash-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-slash arg))

(defun ar-slashparen-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-slashparen arg))

(defun ar-sort-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-sort arg))

(defun ar-trim-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-trim arg))

(defun ar-trim-left-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-trim-left arg))

(defun ar-trim-right-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-trim-right arg))

(defun ar-underscore-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-underscore arg))

(defun ar-whitespace-singlequoted-in-bracketed-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'bracketed 'ar-th-whitespace arg))

(defun ar-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of  at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th arg))

(defun ar-greaterangle-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-lesserangle arg))

(defun ar-backslash-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-backslash arg))

(defun ar-backtick-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-backtick arg))

(defun ar-beg-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-beg arg))

(defun ar-blok-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-blok arg))

(defun ar-bounds-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-bounds arg))

(defun ar-brace-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-brace arg))

(defun ar-bracket-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-bracket arg))

(defun ar-commatize-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-commatize arg))

(defun ar-comment-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-comment arg))

(defun ar-dollar-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-dollar arg))

(defun ar-double-backslash-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-double-backslash arg))

(defun ar-doublequote-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-doublequote arg))

(defun ar-doubleslash-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-double-backslash-paren arg))

(defun ar-end-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of END at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-end arg))

(defun ar-escape-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-escape arg))

(defun ar-hide-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-hide arg))

(defun ar-hide-show-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-hide-show arg))

(defun ar-hyphen-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-hyphen arg))

(defun ar-kill-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-left-right-singlequote arg))

(defun ar-length-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-length arg))

(defun ar-parentize-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-parentize arg))

(defun ar-quote-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-quote arg))

(defun ar-separate-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-separate arg))

(defun ar-show-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-show arg))

(defun ar-singlequote-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-singlequote arg))

(defun ar-slash-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-slash arg))

(defun ar-slashparen-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-slashparen arg))

(defun ar-sort-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-sort arg))

(defun ar-trim-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-trim arg))

(defun ar-trim-left-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-trim-left arg))

(defun ar-trim-right-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-trim-right arg))

(defun ar-underscore-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-underscore arg))

(defun ar-whitespace-singlequoted-in-lesserangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'lesserangled 'ar-th-whitespace arg))

(defun ar-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of  at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th arg))

(defun ar-greaterangle-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-lesserangle arg))

(defun ar-backslash-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-backslash arg))

(defun ar-backtick-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-backtick arg))

(defun ar-beg-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-beg arg))

(defun ar-blok-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-blok arg))

(defun ar-bounds-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-bounds arg))

(defun ar-brace-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-brace arg))

(defun ar-bracket-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-bracket arg))

(defun ar-commatize-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-commatize arg))

(defun ar-comment-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-comment arg))

(defun ar-dollar-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-dollar arg))

(defun ar-double-backslash-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-double-backslash arg))

(defun ar-doublequote-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-doublequote arg))

(defun ar-doubleslash-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-double-backslash-paren arg))

(defun ar-end-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of END at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-end arg))

(defun ar-escape-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-escape arg))

(defun ar-hide-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-hide arg))

(defun ar-hide-show-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-hide-show arg))

(defun ar-hyphen-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-hyphen arg))

(defun ar-kill-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-left-right-singlequote arg))

(defun ar-length-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-length arg))

(defun ar-parentize-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-parentize arg))

(defun ar-quote-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-quote arg))

(defun ar-separate-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-separate arg))

(defun ar-show-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-show arg))

(defun ar-singlequote-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-singlequote arg))

(defun ar-slash-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-slash arg))

(defun ar-slashparen-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-slashparen arg))

(defun ar-sort-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-sort arg))

(defun ar-trim-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-trim arg))

(defun ar-trim-left-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-trim-left arg))

(defun ar-trim-right-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-trim-right arg))

(defun ar-underscore-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-underscore arg))

(defun ar-whitespace-singlequoted-in-greaterangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'greaterangled 'ar-th-whitespace arg))

(defun ar-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of  at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th arg))

(defun ar-greaterangle-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-greaterangle arg))

(defun ar-lesserangle-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-lesserangle arg))

(defun ar-backslash-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-backslash arg))

(defun ar-backtick-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-backtick arg))

(defun ar-beg-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-beg arg))

(defun ar-blok-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-blok arg))

(defun ar-bounds-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-bounds arg))

(defun ar-brace-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-brace arg))

(defun ar-bracket-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-bracket arg))

(defun ar-commatize-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-commatize arg))

(defun ar-comment-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-comment arg))

(defun ar-dollar-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-dollar arg))

(defun ar-double-backslash-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-double-backslash arg))

(defun ar-doublequote-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-doublequote arg))

(defun ar-doubleslash-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-double-backslash-paren arg))

(defun ar-end-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of END at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-end arg))

(defun ar-escape-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-escape arg))

(defun ar-hide-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-hide arg))

(defun ar-hide-show-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-hide-show arg))

(defun ar-hyphen-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-hyphen arg))

(defun ar-kill-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-kill arg))

(defun ar-left-right-singlequote-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-left-right-singlequote arg))

(defun ar-length-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-length arg))

(defun ar-parentize-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-parentize arg))

(defun ar-quote-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-quote arg))

(defun ar-separate-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-separate arg))

(defun ar-show-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-show arg))

(defun ar-singlequote-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-singlequote arg))

(defun ar-slash-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-slash arg))

(defun ar-slashparen-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-slashparen arg))

(defun ar-sort-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-sort arg))

(defun ar-trim-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-trim arg))

(defun ar-trim-left-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-trim-left arg))

(defun ar-trim-right-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-trim-right arg))

(defun ar-underscore-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-underscore arg))

(defun ar-whitespace-singlequoted-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'left-right-singlequoted 'ar-th-whitespace arg))

(defun ar-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of  at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th arg))

(defun ar-greaterangle-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-greaterangle arg))

(defun ar-lesserangle-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-lesserangle arg))

(defun ar-backslash-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-backslash arg))

(defun ar-backtick-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-backtick arg))

(defun ar-beg-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-beg arg))

(defun ar-blok-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-blok arg))

(defun ar-bounds-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-bounds arg))

(defun ar-brace-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-brace arg))

(defun ar-bracket-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-bracket arg))

(defun ar-commatize-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-commatize arg))

(defun ar-comment-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-comment arg))

(defun ar-dollar-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-dollar arg))

(defun ar-double-backslash-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-double-backslash arg))

(defun ar-doublequote-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-doublequote arg))

(defun ar-doubleslash-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-double-backslash-paren arg))

(defun ar-end-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of END at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-end arg))

(defun ar-escape-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-escape arg))

(defun ar-hide-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-hide arg))

(defun ar-hide-show-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-hide-show arg))

(defun ar-hyphen-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-hyphen arg))

(defun ar-kill-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-kill arg))

(defun ar-left-right-singlequote-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-left-right-singlequote arg))

(defun ar-length-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-length arg))

(defun ar-parentize-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-parentize arg))

(defun ar-quote-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-quote arg))

(defun ar-separate-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-separate arg))

(defun ar-show-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-show arg))

(defun ar-singlequote-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-singlequote arg))

(defun ar-slash-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-slash arg))

(defun ar-slashparen-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-slashparen arg))

(defun ar-sort-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-sort arg))

(defun ar-trim-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-trim arg))

(defun ar-trim-left-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-trim-left arg))

(defun ar-trim-right-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-trim-right arg))

(defun ar-underscore-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-underscore arg))

(defun ar-whitespace-singlequoted-in-parentized-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'singlequoted 'parentized 'ar-th-whitespace arg))

(defun ar-slashed-in-braced-atpt (&optional arg)
  "Employ actions of  at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th arg))

(defun ar-greaterangle-slashed-in-braced-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-greaterangle arg))

(defun ar-lesserangle-slashed-in-braced-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-lesserangle arg))

(defun ar-backslash-slashed-in-braced-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-backslash arg))

(defun ar-backtick-slashed-in-braced-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-backtick arg))

(defun ar-beg-slashed-in-braced-atpt (&optional arg)
  "Employ actions of BEG at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-beg arg))

(defun ar-blok-slashed-in-braced-atpt (&optional arg)
  "Employ actions of BLOK at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-blok arg))

(defun ar-bounds-slashed-in-braced-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-bounds arg))

(defun ar-brace-slashed-in-braced-atpt (&optional arg)
  "Employ actions of BRACE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-brace arg))

(defun ar-bracket-slashed-in-braced-atpt (&optional arg)
  "Employ actions of BRACKET at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-bracket arg))

(defun ar-commatize-slashed-in-braced-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-commatize arg))

(defun ar-comment-slashed-in-braced-atpt (&optional arg)
  "Employ actions of COMMENT at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-comment arg))

(defun ar-dollar-slashed-in-braced-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-dollar arg))

(defun ar-double-backslash-slashed-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-double-backslash arg))

(defun ar-doublequote-slashed-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-doublequote arg))

(defun ar-doubleslash-slashed-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-slashed-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-double-backslash-paren arg))

(defun ar-end-slashed-in-braced-atpt (&optional arg)
  "Employ actions of END at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-end arg))

(defun ar-escape-slashed-in-braced-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-escape arg))

(defun ar-hide-slashed-in-braced-atpt (&optional arg)
  "Employ actions of HIDE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-hide arg))

(defun ar-hide-show-slashed-in-braced-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-hide-show arg))

(defun ar-hyphen-slashed-in-braced-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-hyphen arg))

(defun ar-kill-slashed-in-braced-atpt (&optional arg)
  "Employ actions of KILL at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-kill arg))

(defun ar-left-right-singlequote-slashed-in-braced-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-left-right-singlequote arg))

(defun ar-length-slashed-in-braced-atpt (&optional arg)
  "Employ actions of LENGTH at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-length arg))

(defun ar-parentize-slashed-in-braced-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-parentize arg))

(defun ar-quote-slashed-in-braced-atpt (&optional arg)
  "Employ actions of QUOTE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-quote arg))

(defun ar-separate-slashed-in-braced-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-separate arg))

(defun ar-show-slashed-in-braced-atpt (&optional arg)
  "Employ actions of SHOW at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-show arg))

(defun ar-singlequote-slashed-in-braced-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-singlequote arg))

(defun ar-slash-slashed-in-braced-atpt (&optional arg)
  "Employ actions of SLASH at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-slash arg))

(defun ar-slashparen-slashed-in-braced-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-slashparen arg))

(defun ar-sort-slashed-in-braced-atpt (&optional arg)
  "Employ actions of SORT at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-sort arg))

(defun ar-trim-slashed-in-braced-atpt (&optional arg)
  "Employ actions of TRIM at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-trim arg))

(defun ar-trim-left-slashed-in-braced-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-trim-left arg))

(defun ar-trim-right-slashed-in-braced-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-trim-right arg))

(defun ar-underscore-slashed-in-braced-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-underscore arg))

(defun ar-whitespace-slashed-in-braced-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of SLASHED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'braced 'ar-th-whitespace arg))

(defun ar-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of  at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th arg))

(defun ar-greaterangle-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-greaterangle arg))

(defun ar-lesserangle-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-lesserangle arg))

(defun ar-backslash-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-backslash arg))

(defun ar-backtick-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-backtick arg))

(defun ar-beg-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BEG at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-beg arg))

(defun ar-blok-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BLOK at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-blok arg))

(defun ar-bounds-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-bounds arg))

(defun ar-brace-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-brace arg))

(defun ar-bracket-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACKET at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-bracket arg))

(defun ar-commatize-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-commatize arg))

(defun ar-comment-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMENT at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-comment arg))

(defun ar-dollar-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-dollar arg))

(defun ar-double-backslash-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-double-backslash arg))

(defun ar-doublequote-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-doublequote arg))

(defun ar-doubleslash-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-double-backslash-paren arg))

(defun ar-end-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of END at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-end arg))

(defun ar-escape-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-escape arg))

(defun ar-hide-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-hide arg))

(defun ar-hide-show-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-hide-show arg))

(defun ar-hyphen-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-hyphen arg))

(defun ar-kill-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of KILL at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-kill arg))

(defun ar-left-right-singlequote-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-left-right-singlequote arg))

(defun ar-length-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of LENGTH at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-length arg))

(defun ar-parentize-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-parentize arg))

(defun ar-quote-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of QUOTE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-quote arg))

(defun ar-separate-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-separate arg))

(defun ar-show-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of SHOW at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-show arg))

(defun ar-singlequote-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-singlequote arg))

(defun ar-slash-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASH at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-slash arg))

(defun ar-slashparen-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-slashparen arg))

(defun ar-sort-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of SORT at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-sort arg))

(defun ar-trim-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIM at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-trim arg))

(defun ar-trim-left-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-trim-left arg))

(defun ar-trim-right-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-trim-right arg))

(defun ar-underscore-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-underscore arg))

(defun ar-whitespace-slashed-in-bracketed-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of SLASHED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'bracketed 'ar-th-whitespace arg))

(defun ar-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of  at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th arg))

(defun ar-greaterangle-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-lesserangle arg))

(defun ar-backslash-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-backslash arg))

(defun ar-backtick-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-backtick arg))

(defun ar-beg-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BEG at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-beg arg))

(defun ar-blok-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-blok arg))

(defun ar-bounds-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-bounds arg))

(defun ar-brace-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-brace arg))

(defun ar-bracket-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-bracket arg))

(defun ar-commatize-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-commatize arg))

(defun ar-comment-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-comment arg))

(defun ar-dollar-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-dollar arg))

(defun ar-double-backslash-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-double-backslash arg))

(defun ar-doublequote-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-doublequote arg))

(defun ar-doubleslash-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-double-backslash-paren arg))

(defun ar-end-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of END at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-end arg))

(defun ar-escape-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-escape arg))

(defun ar-hide-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-hide arg))

(defun ar-hide-show-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-hide-show arg))

(defun ar-hyphen-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-hyphen arg))

(defun ar-kill-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of KILL at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-left-right-singlequote arg))

(defun ar-length-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-length arg))

(defun ar-parentize-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-parentize arg))

(defun ar-quote-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-quote arg))

(defun ar-separate-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-separate arg))

(defun ar-show-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-show arg))

(defun ar-singlequote-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-singlequote arg))

(defun ar-slash-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-slash arg))

(defun ar-slashparen-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-slashparen arg))

(defun ar-sort-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of SORT at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-sort arg))

(defun ar-trim-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-trim arg))

(defun ar-trim-left-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-trim-left arg))

(defun ar-trim-right-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-trim-right arg))

(defun ar-underscore-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-underscore arg))

(defun ar-whitespace-slashed-in-lesserangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of SLASHED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'lesserangled 'ar-th-whitespace arg))

(defun ar-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of  at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th arg))

(defun ar-greaterangle-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-lesserangle arg))

(defun ar-backslash-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-backslash arg))

(defun ar-backtick-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-backtick arg))

(defun ar-beg-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BEG at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-beg arg))

(defun ar-blok-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-blok arg))

(defun ar-bounds-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-bounds arg))

(defun ar-brace-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-brace arg))

(defun ar-bracket-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-bracket arg))

(defun ar-commatize-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-commatize arg))

(defun ar-comment-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-comment arg))

(defun ar-dollar-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-dollar arg))

(defun ar-double-backslash-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-double-backslash arg))

(defun ar-doublequote-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-doublequote arg))

(defun ar-doubleslash-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-double-backslash-paren arg))

(defun ar-end-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of END at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-end arg))

(defun ar-escape-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-escape arg))

(defun ar-hide-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-hide arg))

(defun ar-hide-show-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-hide-show arg))

(defun ar-hyphen-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-hyphen arg))

(defun ar-kill-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of KILL at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-left-right-singlequote arg))

(defun ar-length-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-length arg))

(defun ar-parentize-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-parentize arg))

(defun ar-quote-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-quote arg))

(defun ar-separate-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-separate arg))

(defun ar-show-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-show arg))

(defun ar-singlequote-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-singlequote arg))

(defun ar-slash-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-slash arg))

(defun ar-slashparen-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-slashparen arg))

(defun ar-sort-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of SORT at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-sort arg))

(defun ar-trim-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-trim arg))

(defun ar-trim-left-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-trim-left arg))

(defun ar-trim-right-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-trim-right arg))

(defun ar-underscore-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-underscore arg))

(defun ar-whitespace-slashed-in-greaterangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of SLASHED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'greaterangled 'ar-th-whitespace arg))

(defun ar-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of  at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th arg))

(defun ar-greaterangle-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-greaterangle arg))

(defun ar-lesserangle-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-lesserangle arg))

(defun ar-backslash-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-backslash arg))

(defun ar-backtick-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-backtick arg))

(defun ar-beg-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BEG at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-beg arg))

(defun ar-blok-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BLOK at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-blok arg))

(defun ar-bounds-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-bounds arg))

(defun ar-brace-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-brace arg))

(defun ar-bracket-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACKET at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-bracket arg))

(defun ar-commatize-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-commatize arg))

(defun ar-comment-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMENT at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-comment arg))

(defun ar-dollar-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-dollar arg))

(defun ar-double-backslash-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-double-backslash arg))

(defun ar-doublequote-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-doublequote arg))

(defun ar-doubleslash-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-double-backslash-paren arg))

(defun ar-end-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of END at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-end arg))

(defun ar-escape-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-escape arg))

(defun ar-hide-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-hide arg))

(defun ar-hide-show-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-hide-show arg))

(defun ar-hyphen-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-hyphen arg))

(defun ar-kill-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of KILL at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-kill arg))

(defun ar-left-right-singlequote-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-left-right-singlequote arg))

(defun ar-length-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LENGTH at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-length arg))

(defun ar-parentize-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-parentize arg))

(defun ar-quote-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of QUOTE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-quote arg))

(defun ar-separate-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-separate arg))

(defun ar-show-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SHOW at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-show arg))

(defun ar-singlequote-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-singlequote arg))

(defun ar-slash-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASH at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-slash arg))

(defun ar-slashparen-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-slashparen arg))

(defun ar-sort-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SORT at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-sort arg))

(defun ar-trim-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIM at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-trim arg))

(defun ar-trim-left-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-trim-left arg))

(defun ar-trim-right-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-trim-right arg))

(defun ar-underscore-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-underscore arg))

(defun ar-whitespace-slashed-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of SLASHED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'left-right-singlequoted 'ar-th-whitespace arg))

(defun ar-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of  at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th arg))

(defun ar-greaterangle-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-greaterangle arg))

(defun ar-lesserangle-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-lesserangle arg))

(defun ar-backslash-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-backslash arg))

(defun ar-backtick-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-backtick arg))

(defun ar-beg-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of BEG at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-beg arg))

(defun ar-blok-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of BLOK at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-blok arg))

(defun ar-bounds-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-bounds arg))

(defun ar-brace-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of BRACE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-brace arg))

(defun ar-bracket-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of BRACKET at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-bracket arg))

(defun ar-commatize-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-commatize arg))

(defun ar-comment-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of COMMENT at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-comment arg))

(defun ar-dollar-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-dollar arg))

(defun ar-double-backslash-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-double-backslash arg))

(defun ar-doublequote-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-doublequote arg))

(defun ar-doubleslash-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-double-backslash-paren arg))

(defun ar-end-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of END at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-end arg))

(defun ar-escape-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-escape arg))

(defun ar-hide-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of HIDE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-hide arg))

(defun ar-hide-show-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-hide-show arg))

(defun ar-hyphen-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-hyphen arg))

(defun ar-kill-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of KILL at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-kill arg))

(defun ar-left-right-singlequote-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-left-right-singlequote arg))

(defun ar-length-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of LENGTH at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-length arg))

(defun ar-parentize-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-parentize arg))

(defun ar-quote-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of QUOTE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-quote arg))

(defun ar-separate-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-separate arg))

(defun ar-show-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of SHOW at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-show arg))

(defun ar-singlequote-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-singlequote arg))

(defun ar-slash-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of SLASH at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-slash arg))

(defun ar-slashparen-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-slashparen arg))

(defun ar-sort-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of SORT at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-sort arg))

(defun ar-trim-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of TRIM at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-trim arg))

(defun ar-trim-left-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-trim-left arg))

(defun ar-trim-right-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-trim-right arg))

(defun ar-underscore-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-underscore arg))

(defun ar-whitespace-slashed-in-parentized-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of SLASHED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'slashed 'parentized 'ar-th-whitespace arg))

(defun ar-underscored-in-braced-atpt (&optional arg)
  "Employ actions of  at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th arg))

(defun ar-greaterangle-underscored-in-braced-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-greaterangle arg))

(defun ar-lesserangle-underscored-in-braced-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-lesserangle arg))

(defun ar-backslash-underscored-in-braced-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-backslash arg))

(defun ar-backtick-underscored-in-braced-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-backtick arg))

(defun ar-beg-underscored-in-braced-atpt (&optional arg)
  "Employ actions of BEG at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-beg arg))

(defun ar-blok-underscored-in-braced-atpt (&optional arg)
  "Employ actions of BLOK at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-blok arg))

(defun ar-bounds-underscored-in-braced-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-bounds arg))

(defun ar-brace-underscored-in-braced-atpt (&optional arg)
  "Employ actions of BRACE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-brace arg))

(defun ar-bracket-underscored-in-braced-atpt (&optional arg)
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-bracket arg))

(defun ar-commatize-underscored-in-braced-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-commatize arg))

(defun ar-comment-underscored-in-braced-atpt (&optional arg)
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-comment arg))

(defun ar-dollar-underscored-in-braced-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-dollar arg))

(defun ar-double-backslash-underscored-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-double-backslash arg))

(defun ar-doublequote-underscored-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-doublequote arg))

(defun ar-doubleslash-underscored-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-underscored-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-double-backslash-paren arg))

(defun ar-end-underscored-in-braced-atpt (&optional arg)
  "Employ actions of END at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-end arg))

(defun ar-escape-underscored-in-braced-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-escape arg))

(defun ar-hide-underscored-in-braced-atpt (&optional arg)
  "Employ actions of HIDE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-hide arg))

(defun ar-hide-show-underscored-in-braced-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-hide-show arg))

(defun ar-hyphen-underscored-in-braced-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-hyphen arg))

(defun ar-kill-underscored-in-braced-atpt (&optional arg)
  "Employ actions of KILL at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-kill arg))

(defun ar-left-right-singlequote-underscored-in-braced-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-left-right-singlequote arg))

(defun ar-length-underscored-in-braced-atpt (&optional arg)
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-length arg))

(defun ar-parentize-underscored-in-braced-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-parentize arg))

(defun ar-quote-underscored-in-braced-atpt (&optional arg)
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-quote arg))

(defun ar-separate-underscored-in-braced-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-separate arg))

(defun ar-show-underscored-in-braced-atpt (&optional arg)
  "Employ actions of SHOW at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-show arg))

(defun ar-singlequote-underscored-in-braced-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-singlequote arg))

(defun ar-slash-underscored-in-braced-atpt (&optional arg)
  "Employ actions of SLASH at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-slash arg))

(defun ar-slashparen-underscored-in-braced-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-slashparen arg))

(defun ar-sort-underscored-in-braced-atpt (&optional arg)
  "Employ actions of SORT at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-sort arg))

(defun ar-trim-underscored-in-braced-atpt (&optional arg)
  "Employ actions of TRIM at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-trim arg))

(defun ar-trim-left-underscored-in-braced-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-trim-left arg))

(defun ar-trim-right-underscored-in-braced-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-trim-right arg))

(defun ar-underscore-underscored-in-braced-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-underscore arg))

(defun ar-whitespace-underscored-in-braced-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'braced 'ar-th-whitespace arg))

(defun ar-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of  at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th arg))

(defun ar-greaterangle-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-greaterangle arg))

(defun ar-lesserangle-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-lesserangle arg))

(defun ar-backslash-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-backslash arg))

(defun ar-backtick-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-backtick arg))

(defun ar-beg-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of BEG at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-beg arg))

(defun ar-blok-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of BLOK at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-blok arg))

(defun ar-bounds-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-bounds arg))

(defun ar-brace-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-brace arg))

(defun ar-bracket-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-bracket arg))

(defun ar-commatize-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-commatize arg))

(defun ar-comment-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-comment arg))

(defun ar-dollar-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-dollar arg))

(defun ar-double-backslash-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-double-backslash arg))

(defun ar-doublequote-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-doublequote arg))

(defun ar-doubleslash-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-double-backslash-paren arg))

(defun ar-end-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of END at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-end arg))

(defun ar-escape-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-escape arg))

(defun ar-hide-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-hide arg))

(defun ar-hide-show-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-hide-show arg))

(defun ar-hyphen-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-hyphen arg))

(defun ar-kill-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of KILL at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-kill arg))

(defun ar-left-right-singlequote-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-left-right-singlequote arg))

(defun ar-length-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-length arg))

(defun ar-parentize-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-parentize arg))

(defun ar-quote-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-quote arg))

(defun ar-separate-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-separate arg))

(defun ar-show-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of SHOW at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-show arg))

(defun ar-singlequote-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-singlequote arg))

(defun ar-slash-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASH at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-slash arg))

(defun ar-slashparen-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-slashparen arg))

(defun ar-sort-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of SORT at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-sort arg))

(defun ar-trim-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIM at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-trim arg))

(defun ar-trim-left-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-trim-left arg))

(defun ar-trim-right-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-trim-right arg))

(defun ar-underscore-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-underscore arg))

(defun ar-whitespace-underscored-in-bracketed-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'bracketed 'ar-th-whitespace arg))

(defun ar-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of  at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th arg))

(defun ar-greaterangle-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-lesserangle arg))

(defun ar-backslash-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-backslash arg))

(defun ar-backtick-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-backtick arg))

(defun ar-beg-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of BEG at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-beg arg))

(defun ar-blok-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-blok arg))

(defun ar-bounds-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-bounds arg))

(defun ar-brace-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-brace arg))

(defun ar-bracket-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-bracket arg))

(defun ar-commatize-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-commatize arg))

(defun ar-comment-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-comment arg))

(defun ar-dollar-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-dollar arg))

(defun ar-double-backslash-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-double-backslash arg))

(defun ar-doublequote-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-doublequote arg))

(defun ar-doubleslash-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-double-backslash-paren arg))

(defun ar-end-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of END at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-end arg))

(defun ar-escape-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-escape arg))

(defun ar-hide-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-hide arg))

(defun ar-hide-show-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-hide-show arg))

(defun ar-hyphen-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-hyphen arg))

(defun ar-kill-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of KILL at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-left-right-singlequote arg))

(defun ar-length-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-length arg))

(defun ar-parentize-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-parentize arg))

(defun ar-quote-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-quote arg))

(defun ar-separate-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-separate arg))

(defun ar-show-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-show arg))

(defun ar-singlequote-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-singlequote arg))

(defun ar-slash-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-slash arg))

(defun ar-slashparen-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-slashparen arg))

(defun ar-sort-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of SORT at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-sort arg))

(defun ar-trim-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-trim arg))

(defun ar-trim-left-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-trim-left arg))

(defun ar-trim-right-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-trim-right arg))

(defun ar-underscore-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-underscore arg))

(defun ar-whitespace-underscored-in-lesserangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'lesserangled 'ar-th-whitespace arg))

(defun ar-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of  at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th arg))

(defun ar-greaterangle-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-lesserangle arg))

(defun ar-backslash-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-backslash arg))

(defun ar-backtick-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-backtick arg))

(defun ar-beg-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of BEG at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-beg arg))

(defun ar-blok-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-blok arg))

(defun ar-bounds-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-bounds arg))

(defun ar-brace-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-brace arg))

(defun ar-bracket-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-bracket arg))

(defun ar-commatize-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-commatize arg))

(defun ar-comment-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-comment arg))

(defun ar-dollar-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-dollar arg))

(defun ar-double-backslash-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-double-backslash arg))

(defun ar-doublequote-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-doublequote arg))

(defun ar-doubleslash-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-double-backslash-paren arg))

(defun ar-end-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of END at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-end arg))

(defun ar-escape-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-escape arg))

(defun ar-hide-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-hide arg))

(defun ar-hide-show-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-hide-show arg))

(defun ar-hyphen-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-hyphen arg))

(defun ar-kill-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of KILL at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-left-right-singlequote arg))

(defun ar-length-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-length arg))

(defun ar-parentize-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-parentize arg))

(defun ar-quote-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-quote arg))

(defun ar-separate-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-separate arg))

(defun ar-show-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-show arg))

(defun ar-singlequote-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-singlequote arg))

(defun ar-slash-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-slash arg))

(defun ar-slashparen-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-slashparen arg))

(defun ar-sort-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of SORT at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-sort arg))

(defun ar-trim-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-trim arg))

(defun ar-trim-left-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-trim-left arg))

(defun ar-trim-right-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-trim-right arg))

(defun ar-underscore-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-underscore arg))

(defun ar-whitespace-underscored-in-greaterangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'greaterangled 'ar-th-whitespace arg))

(defun ar-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of  at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th arg))

(defun ar-greaterangle-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-greaterangle arg))

(defun ar-lesserangle-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-lesserangle arg))

(defun ar-backslash-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-backslash arg))

(defun ar-backtick-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-backtick arg))

(defun ar-beg-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BEG at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-beg arg))

(defun ar-blok-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BLOK at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-blok arg))

(defun ar-bounds-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-bounds arg))

(defun ar-brace-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-brace arg))

(defun ar-bracket-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-bracket arg))

(defun ar-commatize-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-commatize arg))

(defun ar-comment-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-comment arg))

(defun ar-dollar-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-dollar arg))

(defun ar-double-backslash-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-double-backslash arg))

(defun ar-doublequote-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-doublequote arg))

(defun ar-doubleslash-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-double-backslash-paren arg))

(defun ar-end-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of END at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-end arg))

(defun ar-escape-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-escape arg))

(defun ar-hide-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-hide arg))

(defun ar-hide-show-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-hide-show arg))

(defun ar-hyphen-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-hyphen arg))

(defun ar-kill-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of KILL at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-kill arg))

(defun ar-left-right-singlequote-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-left-right-singlequote arg))

(defun ar-length-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-length arg))

(defun ar-parentize-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-parentize arg))

(defun ar-quote-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-quote arg))

(defun ar-separate-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-separate arg))

(defun ar-show-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SHOW at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-show arg))

(defun ar-singlequote-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-singlequote arg))

(defun ar-slash-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASH at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-slash arg))

(defun ar-slashparen-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-slashparen arg))

(defun ar-sort-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SORT at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-sort arg))

(defun ar-trim-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIM at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-trim arg))

(defun ar-trim-left-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-trim-left arg))

(defun ar-trim-right-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-trim-right arg))

(defun ar-underscore-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-underscore arg))

(defun ar-whitespace-underscored-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'left-right-singlequoted 'ar-th-whitespace arg))

(defun ar-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of  at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th arg))

(defun ar-greaterangle-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-greaterangle arg))

(defun ar-lesserangle-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-lesserangle arg))

(defun ar-backslash-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-backslash arg))

(defun ar-backtick-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-backtick arg))

(defun ar-beg-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of BEG at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-beg arg))

(defun ar-blok-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of BLOK at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-blok arg))

(defun ar-bounds-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-bounds arg))

(defun ar-brace-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of BRACE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-brace arg))

(defun ar-bracket-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-bracket arg))

(defun ar-commatize-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-commatize arg))

(defun ar-comment-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-comment arg))

(defun ar-dollar-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-dollar arg))

(defun ar-double-backslash-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-double-backslash arg))

(defun ar-doublequote-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-doublequote arg))

(defun ar-doubleslash-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-double-backslash-paren arg))

(defun ar-end-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of END at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-end arg))

(defun ar-escape-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-escape arg))

(defun ar-hide-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of HIDE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-hide arg))

(defun ar-hide-show-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-hide-show arg))

(defun ar-hyphen-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-hyphen arg))

(defun ar-kill-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of KILL at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-kill arg))

(defun ar-left-right-singlequote-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-left-right-singlequote arg))

(defun ar-length-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-length arg))

(defun ar-parentize-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-parentize arg))

(defun ar-quote-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-quote arg))

(defun ar-separate-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-separate arg))

(defun ar-show-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of SHOW at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-show arg))

(defun ar-singlequote-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-singlequote arg))

(defun ar-slash-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of SLASH at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-slash arg))

(defun ar-slashparen-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-slashparen arg))

(defun ar-sort-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of SORT at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-sort arg))

(defun ar-trim-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of TRIM at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-trim arg))

(defun ar-trim-left-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-trim-left arg))

(defun ar-trim-right-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-trim-right arg))

(defun ar-underscore-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-underscore arg))

(defun ar-whitespace-underscored-in-parentized-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'underscored 'parentized 'ar-th-whitespace arg))

(defun ar-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of  at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th arg))

(defun ar-greaterangle-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-greaterangle arg))

(defun ar-lesserangle-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-lesserangle arg))

(defun ar-backslash-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-backslash arg))

(defun ar-backtick-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-backtick arg))

(defun ar-beg-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of BEG at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-beg arg))

(defun ar-blok-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of BLOK at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-blok arg))

(defun ar-bounds-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-bounds arg))

(defun ar-brace-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of BRACE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-brace arg))

(defun ar-bracket-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of BRACKET at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-bracket arg))

(defun ar-commatize-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-commatize arg))

(defun ar-comment-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of COMMENT at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-comment arg))

(defun ar-dollar-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-dollar arg))

(defun ar-double-backslash-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-double-backslash arg))

(defun ar-doublequote-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-doublequote arg))

(defun ar-doubleslash-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-double-backslash-paren arg))

(defun ar-end-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of END at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-end arg))

(defun ar-escape-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-escape arg))

(defun ar-hide-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of HIDE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-hide arg))

(defun ar-hide-show-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-hide-show arg))

(defun ar-hyphen-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-hyphen arg))

(defun ar-kill-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of KILL at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-kill arg))

(defun ar-left-right-singlequote-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-left-right-singlequote arg))

(defun ar-length-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of LENGTH at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-length arg))

(defun ar-parentize-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-parentize arg))

(defun ar-quote-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of QUOTE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-quote arg))

(defun ar-separate-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-separate arg))

(defun ar-show-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of SHOW at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-show arg))

(defun ar-singlequote-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-singlequote arg))

(defun ar-slash-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of SLASH at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-slash arg))

(defun ar-slashparen-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-slashparen arg))

(defun ar-sort-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of SORT at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-sort arg))

(defun ar-trim-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of TRIM at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-trim arg))

(defun ar-trim-left-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-trim-left arg))

(defun ar-trim-right-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-trim-right arg))

(defun ar-underscore-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-underscore arg))

(defun ar-whitespace-whitespaced-in-braced-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing BRACED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'braced 'ar-th-whitespace arg))

(defun ar-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of  at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th arg))

(defun ar-greaterangle-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-greaterangle arg))

(defun ar-lesserangle-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-lesserangle arg))

(defun ar-backslash-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-backslash arg))

(defun ar-backtick-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-backtick arg))

(defun ar-beg-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of BEG at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-beg arg))

(defun ar-blok-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of BLOK at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-blok arg))

(defun ar-bounds-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-bounds arg))

(defun ar-brace-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-brace arg))

(defun ar-bracket-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of BRACKET at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-bracket arg))

(defun ar-commatize-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-commatize arg))

(defun ar-comment-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of COMMENT at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-comment arg))

(defun ar-dollar-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-dollar arg))

(defun ar-double-backslash-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-double-backslash arg))

(defun ar-doublequote-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-doublequote arg))

(defun ar-doubleslash-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-double-backslash-paren arg))

(defun ar-end-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of END at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-end arg))

(defun ar-escape-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-escape arg))

(defun ar-hide-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-hide arg))

(defun ar-hide-show-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-hide-show arg))

(defun ar-hyphen-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-hyphen arg))

(defun ar-kill-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of KILL at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-kill arg))

(defun ar-left-right-singlequote-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-left-right-singlequote arg))

(defun ar-length-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of LENGTH at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-length arg))

(defun ar-parentize-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-parentize arg))

(defun ar-quote-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of QUOTE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-quote arg))

(defun ar-separate-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-separate arg))

(defun ar-show-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of SHOW at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-show arg))

(defun ar-singlequote-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-singlequote arg))

(defun ar-slash-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASH at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-slash arg))

(defun ar-slashparen-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-slashparen arg))

(defun ar-sort-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of SORT at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-sort arg))

(defun ar-trim-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIM at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-trim arg))

(defun ar-trim-left-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-trim-left arg))

(defun ar-trim-right-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-trim-right arg))

(defun ar-underscore-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-underscore arg))

(defun ar-whitespace-whitespaced-in-bracketed-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing BRACKETED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'bracketed 'ar-th-whitespace arg))

(defun ar-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of  at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th arg))

(defun ar-greaterangle-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-lesserangle arg))

(defun ar-backslash-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-backslash arg))

(defun ar-backtick-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-backtick arg))

(defun ar-beg-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of BEG at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-beg arg))

(defun ar-blok-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-blok arg))

(defun ar-bounds-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-bounds arg))

(defun ar-brace-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-brace arg))

(defun ar-bracket-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-bracket arg))

(defun ar-commatize-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-commatize arg))

(defun ar-comment-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-comment arg))

(defun ar-dollar-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-dollar arg))

(defun ar-double-backslash-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-double-backslash arg))

(defun ar-doublequote-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-doublequote arg))

(defun ar-doubleslash-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-double-backslash-paren arg))

(defun ar-end-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of END at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-end arg))

(defun ar-escape-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-escape arg))

(defun ar-hide-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-hide arg))

(defun ar-hide-show-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-hide-show arg))

(defun ar-hyphen-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-hyphen arg))

(defun ar-kill-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of KILL at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-left-right-singlequote arg))

(defun ar-length-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-length arg))

(defun ar-parentize-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-parentize arg))

(defun ar-quote-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-quote arg))

(defun ar-separate-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-separate arg))

(defun ar-show-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-show arg))

(defun ar-singlequote-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-singlequote arg))

(defun ar-slash-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-slash arg))

(defun ar-slashparen-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-slashparen arg))

(defun ar-sort-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of SORT at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-sort arg))

(defun ar-trim-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-trim arg))

(defun ar-trim-left-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-trim-left arg))

(defun ar-trim-right-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-trim-right arg))

(defun ar-underscore-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-underscore arg))

(defun ar-whitespace-whitespaced-in-lesserangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing LESSER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'lesserangled 'ar-th-whitespace arg))

(defun ar-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of  at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th arg))

(defun ar-greaterangle-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-greaterangle arg))

(defun ar-lesserangle-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-lesserangle arg))

(defun ar-backslash-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-backslash arg))

(defun ar-backtick-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-backtick arg))

(defun ar-beg-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of BEG at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-beg arg))

(defun ar-blok-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of BLOK at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-blok arg))

(defun ar-bounds-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-bounds arg))

(defun ar-brace-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-brace arg))

(defun ar-bracket-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of BRACKET at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-bracket arg))

(defun ar-commatize-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-commatize arg))

(defun ar-comment-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of COMMENT at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-comment arg))

(defun ar-dollar-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-dollar arg))

(defun ar-double-backslash-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-double-backslash arg))

(defun ar-doublequote-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-doublequote arg))

(defun ar-doubleslash-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-double-backslash-paren arg))

(defun ar-end-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of END at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-end arg))

(defun ar-escape-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-escape arg))

(defun ar-hide-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-hide arg))

(defun ar-hide-show-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-hide-show arg))

(defun ar-hyphen-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-hyphen arg))

(defun ar-kill-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of KILL at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-kill arg))

(defun ar-left-right-singlequote-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-left-right-singlequote arg))

(defun ar-length-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of LENGTH at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-length arg))

(defun ar-parentize-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-parentize arg))

(defun ar-quote-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of QUOTE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-quote arg))

(defun ar-separate-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-separate arg))

(defun ar-show-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of SHOW at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-show arg))

(defun ar-singlequote-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-singlequote arg))

(defun ar-slash-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASH at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-slash arg))

(defun ar-slashparen-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-slashparen arg))

(defun ar-sort-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of SORT at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-sort arg))

(defun ar-trim-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIM at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-trim arg))

(defun ar-trim-left-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-trim-left arg))

(defun ar-trim-right-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-trim-right arg))

(defun ar-underscore-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-underscore arg))

(defun ar-whitespace-whitespaced-in-greaterangled-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing GREATER-ANGLED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'greaterangled 'ar-th-whitespace arg))

(defun ar-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of  at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th arg))

(defun ar-greaterangle-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-greaterangle arg))

(defun ar-lesserangle-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-lesserangle arg))

(defun ar-backslash-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-backslash arg))

(defun ar-backtick-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-backtick arg))

(defun ar-beg-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BEG at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-beg arg))

(defun ar-blok-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BLOK at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-blok arg))

(defun ar-bounds-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-bounds arg))

(defun ar-brace-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-brace arg))

(defun ar-bracket-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of BRACKET at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-bracket arg))

(defun ar-commatize-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-commatize arg))

(defun ar-comment-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of COMMENT at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-comment arg))

(defun ar-dollar-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-dollar arg))

(defun ar-double-backslash-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-double-backslash arg))

(defun ar-doublequote-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-doublequote arg))

(defun ar-doubleslash-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-double-backslash-paren arg))

(defun ar-end-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of END at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-end arg))

(defun ar-escape-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-escape arg))

(defun ar-hide-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-hide arg))

(defun ar-hide-show-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-hide-show arg))

(defun ar-hyphen-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-hyphen arg))

(defun ar-kill-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of KILL at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-kill arg))

(defun ar-left-right-singlequote-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-left-right-singlequote arg))

(defun ar-length-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of LENGTH at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-length arg))

(defun ar-parentize-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-parentize arg))

(defun ar-quote-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of QUOTE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-quote arg))

(defun ar-separate-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-separate arg))

(defun ar-show-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SHOW at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-show arg))

(defun ar-singlequote-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-singlequote arg))

(defun ar-slash-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASH at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-slash arg))

(defun ar-slashparen-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-slashparen arg))

(defun ar-sort-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of SORT at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-sort arg))

(defun ar-trim-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIM at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-trim arg))

(defun ar-trim-left-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-trim-left arg))

(defun ar-trim-right-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-trim-right arg))

(defun ar-underscore-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-underscore arg))

(defun ar-whitespace-whitespaced-in-left-right-singlequoted-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing LEFT-RIGHT-SINGLEQUOTED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'left-right-singlequoted 'ar-th-whitespace arg))

(defun ar-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of  at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th arg))

(defun ar-greaterangle-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-greaterangle arg))

(defun ar-lesserangle-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-lesserangle arg))

(defun ar-backslash-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-backslash arg))

(defun ar-backtick-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-backtick arg))

(defun ar-beg-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of BEG at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-beg arg))

(defun ar-blok-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of BLOK at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-blok arg))

(defun ar-bounds-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-bounds arg))

(defun ar-brace-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of BRACE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-brace arg))

(defun ar-bracket-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of BRACKET at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-bracket arg))

(defun ar-commatize-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-commatize arg))

(defun ar-comment-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of COMMENT at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-comment arg))

(defun ar-dollar-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-dollar arg))

(defun ar-double-backslash-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-double-backslash arg))

(defun ar-doublequote-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-doublequote arg))

(defun ar-doubleslash-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-doubleslash arg))

(defun ar-double-backslash-paren-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-double-backslash-paren arg))

(defun ar-end-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of END at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-end arg))

(defun ar-escape-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-escape arg))

(defun ar-hide-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of HIDE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-hide arg))

(defun ar-hide-show-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-hide-show arg))

(defun ar-hyphen-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-hyphen arg))

(defun ar-kill-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of KILL at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-kill arg))

(defun ar-left-right-singlequote-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-left-right-singlequote arg))

(defun ar-length-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of LENGTH at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-length arg))

(defun ar-parentize-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-parentize arg))

(defun ar-quote-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of QUOTE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-quote arg))

(defun ar-separate-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-separate arg))

(defun ar-show-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of SHOW at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-show arg))

(defun ar-singlequote-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-singlequote arg))

(defun ar-slash-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of SLASH at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-slash arg))

(defun ar-slashparen-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-slashparen arg))

(defun ar-sort-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of SORT at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-sort arg))

(defun ar-trim-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of TRIM at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-trim arg))

(defun ar-trim-left-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-trim-left arg))

(defun ar-trim-right-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-trim-right arg))

(defun ar-underscore-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-underscore arg))

(defun ar-whitespace-whitespaced-in-parentized-atpt (&optional arg)
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing PARENTIZED."
  (interactive "p*")
  (ar-thing-in-thing 'whitespaced 'parentized 'ar-th-whitespace arg))

(provide 'thing-unpaired-delimited-list-in-delimited-list)
;;;thing-unpaired-delimited-list-in-delimited-list.el ends here

