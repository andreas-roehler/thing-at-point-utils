;;; ar-thingatpt-classes-in-delimited-list.el --- thing-in-thing functions

;; Copyright (C) 2010-2024 Andreas Röhler, unless
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
;; contents lists at the bottom of ar-thingatpt-utils-core.el which are
;; cross-used in general.

;; Further information is given with ar-thingatpt-utils-core.el

(defun ar-alnum-in-braced-atpt ()
  "Employ actions of  at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-alnum-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-alnum-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-alnum-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-alnum-in-braced-atpt ()
  "Employ actions of BEG at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-alnum-in-braced-atpt ()
  "Employ actions of BLOK at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-alnum-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-alnum-in-braced-atpt ()
  "Employ actions of BRACE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-alnum-in-braced-atpt ()
  "Employ actions of BRACKET at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-alnum-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-alnum-in-braced-atpt ()
  "Employ actions of COMMENT at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-alnum-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-alnum-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-alnum-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-alnum-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-alnum-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-alnum-in-braced-atpt ()
  "Employ actions of END at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-alnum-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-alnum-in-braced-atpt ()
  "Employ actions of HIDE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-alnum-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-alnum-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-alnum-in-braced-atpt ()
  "Employ actions of KILL at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-alnum-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-alnum-in-braced-atpt ()
  "Employ actions of LENGTH at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-alnum-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-alnum-in-braced-atpt ()
  "Employ actions of QUOTE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-alnum-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-alnum-in-braced-atpt ()
  "Employ actions of SHOW at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-alnum-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-alnum-in-braced-atpt ()
  "Employ actions of SLASH at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-alnum-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-alnum-in-braced-atpt ()
  "Employ actions of SORT at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-alnum-in-braced-atpt ()
  "Employ actions of TRIM at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-alnum-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-alnum-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-alnum-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-alnum-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of ALNUM residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-alnum-in-bracketed-atpt ()
  "Employ actions of  at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-alnum-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-alnum-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-alnum-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-alnum-in-bracketed-atpt ()
  "Employ actions of BEG at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-alnum-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-alnum-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-alnum-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-alnum-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-alnum-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-alnum-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-alnum-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-alnum-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-alnum-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-alnum-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-alnum-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-alnum-in-bracketed-atpt ()
  "Employ actions of END at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-alnum-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-alnum-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-alnum-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-alnum-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-alnum-in-bracketed-atpt ()
  "Employ actions of KILL at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-alnum-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-alnum-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-alnum-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-alnum-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-alnum-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-alnum-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-alnum-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-alnum-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-alnum-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-alnum-in-bracketed-atpt ()
  "Employ actions of SORT at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-alnum-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-alnum-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-alnum-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-alnum-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-alnum-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of ALNUM residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-alnum-in-lesserangled-atpt ()
  "Employ actions of  at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-alnum-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-alnum-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-alnum-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-alnum-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-alnum-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-alnum-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-alnum-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-alnum-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-alnum-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-alnum-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-alnum-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-alnum-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-alnum-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-alnum-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-alnum-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-alnum-in-lesserangled-atpt ()
  "Employ actions of END at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-alnum-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-alnum-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-alnum-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-alnum-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-alnum-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-alnum-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-alnum-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-alnum-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-alnum-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-alnum-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-alnum-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-alnum-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-alnum-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-alnum-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-alnum-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-alnum-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-alnum-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-alnum-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-alnum-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-alnum-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of ALNUM residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-alnum-in-greaterangled-atpt ()
  "Employ actions of  at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-alnum-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-alnum-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-alnum-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-alnum-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-alnum-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-alnum-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-alnum-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-alnum-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-alnum-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-alnum-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-alnum-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-alnum-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-alnum-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-alnum-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-alnum-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-alnum-in-greaterangled-atpt ()
  "Employ actions of END at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-alnum-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-alnum-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-alnum-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-alnum-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-alnum-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-alnum-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-alnum-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-alnum-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-alnum-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-alnum-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-alnum-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-alnum-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-alnum-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-alnum-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-alnum-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-alnum-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-alnum-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-alnum-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-alnum-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-alnum-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of ALNUM residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-alnum-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of ALNUM residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-alnum-in-parentized-atpt ()
  "Employ actions of  at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-alnum-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-alnum-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-alnum-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-alnum-in-parentized-atpt ()
  "Employ actions of BEG at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-alnum-in-parentized-atpt ()
  "Employ actions of BLOK at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-alnum-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-alnum-in-parentized-atpt ()
  "Employ actions of BRACE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-alnum-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-alnum-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-alnum-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-alnum-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-alnum-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-alnum-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-alnum-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-alnum-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-alnum-in-parentized-atpt ()
  "Employ actions of END at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-alnum-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-alnum-in-parentized-atpt ()
  "Employ actions of HIDE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-alnum-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-alnum-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-alnum-in-parentized-atpt ()
  "Employ actions of KILL at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-alnum-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-alnum-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-alnum-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-alnum-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-alnum-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-alnum-in-parentized-atpt ()
  "Employ actions of SHOW at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-alnum-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-alnum-in-parentized-atpt ()
  "Employ actions of SLASH at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-alnum-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-alnum-in-parentized-atpt ()
  "Employ actions of SORT at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-alnum-in-parentized-atpt ()
  "Employ actions of TRIM at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-alnum-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-alnum-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-alnum-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-alnum-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of ALNUM residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alnum 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-alpha-in-braced-atpt ()
  "Employ actions of  at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-alpha-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-alpha-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-alpha-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-alpha-in-braced-atpt ()
  "Employ actions of BEG at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-alpha-in-braced-atpt ()
  "Employ actions of BLOK at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-alpha-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-alpha-in-braced-atpt ()
  "Employ actions of BRACE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-alpha-in-braced-atpt ()
  "Employ actions of BRACKET at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-alpha-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-alpha-in-braced-atpt ()
  "Employ actions of COMMENT at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-alpha-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-alpha-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-alpha-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-alpha-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-alpha-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-alpha-in-braced-atpt ()
  "Employ actions of END at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-alpha-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-alpha-in-braced-atpt ()
  "Employ actions of HIDE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-alpha-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-alpha-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-alpha-in-braced-atpt ()
  "Employ actions of KILL at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-alpha-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-alpha-in-braced-atpt ()
  "Employ actions of LENGTH at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-alpha-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-alpha-in-braced-atpt ()
  "Employ actions of QUOTE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-alpha-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-alpha-in-braced-atpt ()
  "Employ actions of SHOW at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-alpha-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-alpha-in-braced-atpt ()
  "Employ actions of SLASH at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-alpha-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-alpha-in-braced-atpt ()
  "Employ actions of SORT at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-alpha-in-braced-atpt ()
  "Employ actions of TRIM at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-alpha-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-alpha-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-alpha-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-alpha-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of ALPHA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-alpha-in-bracketed-atpt ()
  "Employ actions of  at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-alpha-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-alpha-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-alpha-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-alpha-in-bracketed-atpt ()
  "Employ actions of BEG at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-alpha-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-alpha-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-alpha-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-alpha-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-alpha-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-alpha-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-alpha-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-alpha-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-alpha-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-alpha-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-alpha-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-alpha-in-bracketed-atpt ()
  "Employ actions of END at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-alpha-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-alpha-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-alpha-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-alpha-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-alpha-in-bracketed-atpt ()
  "Employ actions of KILL at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-alpha-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-alpha-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-alpha-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-alpha-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-alpha-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-alpha-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-alpha-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-alpha-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-alpha-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-alpha-in-bracketed-atpt ()
  "Employ actions of SORT at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-alpha-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-alpha-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-alpha-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-alpha-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-alpha-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of ALPHA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-alpha-in-lesserangled-atpt ()
  "Employ actions of  at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-alpha-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-alpha-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-alpha-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-alpha-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-alpha-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-alpha-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-alpha-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-alpha-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-alpha-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-alpha-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-alpha-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-alpha-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-alpha-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-alpha-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-alpha-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-alpha-in-lesserangled-atpt ()
  "Employ actions of END at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-alpha-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-alpha-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-alpha-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-alpha-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-alpha-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-alpha-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-alpha-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-alpha-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-alpha-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-alpha-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-alpha-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-alpha-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-alpha-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-alpha-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-alpha-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-alpha-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-alpha-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-alpha-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-alpha-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-alpha-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of ALPHA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-alpha-in-greaterangled-atpt ()
  "Employ actions of  at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-alpha-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-alpha-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-alpha-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-alpha-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-alpha-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-alpha-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-alpha-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-alpha-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-alpha-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-alpha-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-alpha-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-alpha-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-alpha-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-alpha-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-alpha-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-alpha-in-greaterangled-atpt ()
  "Employ actions of END at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-alpha-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-alpha-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-alpha-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-alpha-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-alpha-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-alpha-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-alpha-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-alpha-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-alpha-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-alpha-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-alpha-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-alpha-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-alpha-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-alpha-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-alpha-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-alpha-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-alpha-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-alpha-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-alpha-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-alpha-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of ALPHA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-alpha-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of ALPHA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-alpha-in-parentized-atpt ()
  "Employ actions of  at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-alpha-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-alpha-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-alpha-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-alpha-in-parentized-atpt ()
  "Employ actions of BEG at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-alpha-in-parentized-atpt ()
  "Employ actions of BLOK at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-alpha-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-alpha-in-parentized-atpt ()
  "Employ actions of BRACE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-alpha-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-alpha-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-alpha-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-alpha-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-alpha-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-alpha-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-alpha-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-alpha-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-alpha-in-parentized-atpt ()
  "Employ actions of END at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-alpha-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-alpha-in-parentized-atpt ()
  "Employ actions of HIDE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-alpha-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-alpha-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-alpha-in-parentized-atpt ()
  "Employ actions of KILL at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-alpha-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-alpha-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-alpha-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-alpha-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-alpha-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-alpha-in-parentized-atpt ()
  "Employ actions of SHOW at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-alpha-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-alpha-in-parentized-atpt ()
  "Employ actions of SLASH at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-alpha-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-alpha-in-parentized-atpt ()
  "Employ actions of SORT at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-alpha-in-parentized-atpt ()
  "Employ actions of TRIM at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-alpha-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-alpha-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-alpha-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-alpha-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of ALPHA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'alpha 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-ascii-in-braced-atpt ()
  "Employ actions of  at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-ascii-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-ascii-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-ascii-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-ascii-in-braced-atpt ()
  "Employ actions of BEG at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-ascii-in-braced-atpt ()
  "Employ actions of BLOK at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-ascii-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-ascii-in-braced-atpt ()
  "Employ actions of BRACE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-ascii-in-braced-atpt ()
  "Employ actions of BRACKET at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-ascii-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-ascii-in-braced-atpt ()
  "Employ actions of COMMENT at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-ascii-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-ascii-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-ascii-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-ascii-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-ascii-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-ascii-in-braced-atpt ()
  "Employ actions of END at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-ascii-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-ascii-in-braced-atpt ()
  "Employ actions of HIDE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-ascii-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-ascii-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-ascii-in-braced-atpt ()
  "Employ actions of KILL at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-ascii-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-ascii-in-braced-atpt ()
  "Employ actions of LENGTH at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-ascii-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-ascii-in-braced-atpt ()
  "Employ actions of QUOTE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-ascii-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-ascii-in-braced-atpt ()
  "Employ actions of SHOW at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-ascii-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-ascii-in-braced-atpt ()
  "Employ actions of SLASH at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-ascii-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-ascii-in-braced-atpt ()
  "Employ actions of SORT at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-ascii-in-braced-atpt ()
  "Employ actions of TRIM at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-ascii-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-ascii-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-ascii-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-ascii-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of ASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-ascii-in-bracketed-atpt ()
  "Employ actions of  at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-ascii-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-ascii-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-ascii-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-ascii-in-bracketed-atpt ()
  "Employ actions of BEG at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-ascii-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-ascii-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-ascii-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-ascii-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-ascii-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-ascii-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-ascii-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-ascii-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-ascii-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-ascii-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-ascii-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-ascii-in-bracketed-atpt ()
  "Employ actions of END at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-ascii-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-ascii-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-ascii-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-ascii-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-ascii-in-bracketed-atpt ()
  "Employ actions of KILL at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-ascii-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-ascii-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-ascii-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-ascii-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-ascii-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-ascii-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-ascii-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-ascii-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-ascii-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-ascii-in-bracketed-atpt ()
  "Employ actions of SORT at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-ascii-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-ascii-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-ascii-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-ascii-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-ascii-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of ASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-ascii-in-lesserangled-atpt ()
  "Employ actions of  at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-ascii-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-ascii-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-ascii-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-ascii-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-ascii-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-ascii-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-ascii-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-ascii-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-ascii-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-ascii-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-ascii-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-ascii-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-ascii-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-ascii-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-ascii-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-ascii-in-lesserangled-atpt ()
  "Employ actions of END at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-ascii-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-ascii-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-ascii-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-ascii-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-ascii-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-ascii-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-ascii-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-ascii-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-ascii-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-ascii-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-ascii-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-ascii-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-ascii-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-ascii-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-ascii-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-ascii-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-ascii-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-ascii-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-ascii-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-ascii-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of ASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-ascii-in-greaterangled-atpt ()
  "Employ actions of  at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-ascii-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-ascii-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-ascii-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-ascii-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-ascii-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-ascii-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-ascii-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-ascii-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-ascii-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-ascii-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-ascii-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-ascii-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-ascii-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-ascii-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-ascii-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-ascii-in-greaterangled-atpt ()
  "Employ actions of END at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-ascii-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-ascii-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-ascii-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-ascii-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-ascii-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-ascii-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-ascii-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-ascii-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-ascii-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-ascii-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-ascii-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-ascii-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-ascii-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-ascii-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-ascii-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-ascii-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-ascii-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-ascii-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-ascii-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-ascii-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of ASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-ascii-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of ASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-ascii-in-parentized-atpt ()
  "Employ actions of  at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-ascii-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-ascii-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-ascii-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-ascii-in-parentized-atpt ()
  "Employ actions of BEG at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-ascii-in-parentized-atpt ()
  "Employ actions of BLOK at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-ascii-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-ascii-in-parentized-atpt ()
  "Employ actions of BRACE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-ascii-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-ascii-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-ascii-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-ascii-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-ascii-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-ascii-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-ascii-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-ascii-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-ascii-in-parentized-atpt ()
  "Employ actions of END at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-ascii-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-ascii-in-parentized-atpt ()
  "Employ actions of HIDE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-ascii-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-ascii-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-ascii-in-parentized-atpt ()
  "Employ actions of KILL at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-ascii-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-ascii-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-ascii-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-ascii-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-ascii-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-ascii-in-parentized-atpt ()
  "Employ actions of SHOW at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-ascii-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-ascii-in-parentized-atpt ()
  "Employ actions of SLASH at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-ascii-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-ascii-in-parentized-atpt ()
  "Employ actions of SORT at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-ascii-in-parentized-atpt ()
  "Employ actions of TRIM at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-ascii-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-ascii-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-ascii-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-ascii-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of ASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'ascii 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-blank-in-braced-atpt ()
  "Employ actions of  at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-blank-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-blank-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-blank-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-blank-in-braced-atpt ()
  "Employ actions of BEG at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-blank-in-braced-atpt ()
  "Employ actions of BLOK at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-blank-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-blank-in-braced-atpt ()
  "Employ actions of BRACE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-blank-in-braced-atpt ()
  "Employ actions of BRACKET at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-blank-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-blank-in-braced-atpt ()
  "Employ actions of COMMENT at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-blank-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-blank-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-blank-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-blank-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-blank-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-blank-in-braced-atpt ()
  "Employ actions of END at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-blank-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-blank-in-braced-atpt ()
  "Employ actions of HIDE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-blank-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-blank-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-blank-in-braced-atpt ()
  "Employ actions of KILL at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-blank-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-blank-in-braced-atpt ()
  "Employ actions of LENGTH at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-blank-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-blank-in-braced-atpt ()
  "Employ actions of QUOTE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-blank-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-blank-in-braced-atpt ()
  "Employ actions of SHOW at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-blank-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-blank-in-braced-atpt ()
  "Employ actions of SLASH at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-blank-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-blank-in-braced-atpt ()
  "Employ actions of SORT at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-blank-in-braced-atpt ()
  "Employ actions of TRIM at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-blank-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-blank-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-blank-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-blank-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of BLANK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-blank-in-bracketed-atpt ()
  "Employ actions of  at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-blank-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-blank-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-blank-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-blank-in-bracketed-atpt ()
  "Employ actions of BEG at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-blank-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-blank-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-blank-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-blank-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-blank-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-blank-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-blank-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-blank-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-blank-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-blank-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-blank-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-blank-in-bracketed-atpt ()
  "Employ actions of END at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-blank-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-blank-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-blank-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-blank-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-blank-in-bracketed-atpt ()
  "Employ actions of KILL at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-blank-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-blank-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-blank-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-blank-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-blank-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-blank-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-blank-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-blank-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-blank-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-blank-in-bracketed-atpt ()
  "Employ actions of SORT at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-blank-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-blank-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-blank-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-blank-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-blank-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of BLANK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-blank-in-lesserangled-atpt ()
  "Employ actions of  at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-blank-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-blank-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-blank-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-blank-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-blank-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-blank-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-blank-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-blank-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-blank-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-blank-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-blank-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-blank-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-blank-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-blank-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-blank-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-blank-in-lesserangled-atpt ()
  "Employ actions of END at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-blank-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-blank-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-blank-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-blank-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-blank-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-blank-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-blank-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-blank-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-blank-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-blank-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-blank-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-blank-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-blank-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-blank-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-blank-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-blank-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-blank-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-blank-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-blank-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-blank-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of BLANK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-blank-in-greaterangled-atpt ()
  "Employ actions of  at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-blank-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-blank-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-blank-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-blank-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-blank-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-blank-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-blank-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-blank-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-blank-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-blank-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-blank-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-blank-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-blank-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-blank-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-blank-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-blank-in-greaterangled-atpt ()
  "Employ actions of END at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-blank-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-blank-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-blank-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-blank-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-blank-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-blank-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-blank-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-blank-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-blank-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-blank-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-blank-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-blank-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-blank-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-blank-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-blank-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-blank-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-blank-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-blank-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-blank-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-blank-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of BLANK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-blank-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of BLANK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-blank-in-parentized-atpt ()
  "Employ actions of  at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-blank-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-blank-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-blank-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-blank-in-parentized-atpt ()
  "Employ actions of BEG at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-blank-in-parentized-atpt ()
  "Employ actions of BLOK at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-blank-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-blank-in-parentized-atpt ()
  "Employ actions of BRACE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-blank-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-blank-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-blank-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-blank-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-blank-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-blank-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-blank-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-blank-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-blank-in-parentized-atpt ()
  "Employ actions of END at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-blank-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-blank-in-parentized-atpt ()
  "Employ actions of HIDE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-blank-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-blank-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-blank-in-parentized-atpt ()
  "Employ actions of KILL at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-blank-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-blank-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-blank-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-blank-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-blank-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-blank-in-parentized-atpt ()
  "Employ actions of SHOW at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-blank-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-blank-in-parentized-atpt ()
  "Employ actions of SLASH at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-blank-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-blank-in-parentized-atpt ()
  "Employ actions of SORT at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-blank-in-parentized-atpt ()
  "Employ actions of TRIM at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-blank-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-blank-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-blank-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-blank-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of BLANK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blank 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-cntrl-in-braced-atpt ()
  "Employ actions of  at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-cntrl-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-cntrl-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-cntrl-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-cntrl-in-braced-atpt ()
  "Employ actions of BEG at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-cntrl-in-braced-atpt ()
  "Employ actions of BLOK at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-cntrl-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-cntrl-in-braced-atpt ()
  "Employ actions of BRACE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-cntrl-in-braced-atpt ()
  "Employ actions of BRACKET at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-cntrl-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-cntrl-in-braced-atpt ()
  "Employ actions of COMMENT at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-cntrl-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-cntrl-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-cntrl-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-cntrl-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-cntrl-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-cntrl-in-braced-atpt ()
  "Employ actions of END at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-cntrl-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-cntrl-in-braced-atpt ()
  "Employ actions of HIDE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-cntrl-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-cntrl-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-cntrl-in-braced-atpt ()
  "Employ actions of KILL at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-cntrl-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-cntrl-in-braced-atpt ()
  "Employ actions of LENGTH at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-cntrl-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-cntrl-in-braced-atpt ()
  "Employ actions of QUOTE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-cntrl-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-cntrl-in-braced-atpt ()
  "Employ actions of SHOW at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-cntrl-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-cntrl-in-braced-atpt ()
  "Employ actions of SLASH at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-cntrl-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-cntrl-in-braced-atpt ()
  "Employ actions of SORT at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-cntrl-in-braced-atpt ()
  "Employ actions of TRIM at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-cntrl-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-cntrl-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-cntrl-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-cntrl-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of CNTRL residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-cntrl-in-bracketed-atpt ()
  "Employ actions of  at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-cntrl-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-cntrl-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-cntrl-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-cntrl-in-bracketed-atpt ()
  "Employ actions of BEG at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-cntrl-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-cntrl-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-cntrl-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-cntrl-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-cntrl-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-cntrl-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-cntrl-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-cntrl-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-cntrl-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-cntrl-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-cntrl-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-cntrl-in-bracketed-atpt ()
  "Employ actions of END at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-cntrl-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-cntrl-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-cntrl-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-cntrl-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-cntrl-in-bracketed-atpt ()
  "Employ actions of KILL at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-cntrl-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-cntrl-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-cntrl-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-cntrl-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-cntrl-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-cntrl-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-cntrl-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-cntrl-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-cntrl-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-cntrl-in-bracketed-atpt ()
  "Employ actions of SORT at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-cntrl-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-cntrl-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-cntrl-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-cntrl-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-cntrl-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of CNTRL residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-cntrl-in-lesserangled-atpt ()
  "Employ actions of  at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-cntrl-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-cntrl-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-cntrl-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-cntrl-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-cntrl-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-cntrl-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-cntrl-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-cntrl-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-cntrl-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-cntrl-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-cntrl-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-cntrl-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-cntrl-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-cntrl-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-cntrl-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-cntrl-in-lesserangled-atpt ()
  "Employ actions of END at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-cntrl-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-cntrl-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-cntrl-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-cntrl-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-cntrl-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-cntrl-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-cntrl-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-cntrl-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-cntrl-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-cntrl-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-cntrl-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-cntrl-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-cntrl-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-cntrl-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-cntrl-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-cntrl-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-cntrl-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-cntrl-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-cntrl-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-cntrl-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of CNTRL residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-cntrl-in-greaterangled-atpt ()
  "Employ actions of  at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-cntrl-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-cntrl-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-cntrl-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-cntrl-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-cntrl-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-cntrl-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-cntrl-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-cntrl-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-cntrl-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-cntrl-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-cntrl-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-cntrl-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-cntrl-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-cntrl-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-cntrl-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-cntrl-in-greaterangled-atpt ()
  "Employ actions of END at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-cntrl-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-cntrl-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-cntrl-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-cntrl-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-cntrl-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-cntrl-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-cntrl-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-cntrl-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-cntrl-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-cntrl-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-cntrl-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-cntrl-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-cntrl-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-cntrl-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-cntrl-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-cntrl-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-cntrl-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-cntrl-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-cntrl-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-cntrl-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of CNTRL residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-cntrl-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of CNTRL residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-cntrl-in-parentized-atpt ()
  "Employ actions of  at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-cntrl-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-cntrl-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-cntrl-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-cntrl-in-parentized-atpt ()
  "Employ actions of BEG at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-cntrl-in-parentized-atpt ()
  "Employ actions of BLOK at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-cntrl-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-cntrl-in-parentized-atpt ()
  "Employ actions of BRACE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-cntrl-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-cntrl-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-cntrl-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-cntrl-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-cntrl-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-cntrl-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-cntrl-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-cntrl-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-cntrl-in-parentized-atpt ()
  "Employ actions of END at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-cntrl-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-cntrl-in-parentized-atpt ()
  "Employ actions of HIDE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-cntrl-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-cntrl-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-cntrl-in-parentized-atpt ()
  "Employ actions of KILL at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-cntrl-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-cntrl-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-cntrl-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-cntrl-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-cntrl-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-cntrl-in-parentized-atpt ()
  "Employ actions of SHOW at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-cntrl-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-cntrl-in-parentized-atpt ()
  "Employ actions of SLASH at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-cntrl-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-cntrl-in-parentized-atpt ()
  "Employ actions of SORT at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-cntrl-in-parentized-atpt ()
  "Employ actions of TRIM at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-cntrl-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-cntrl-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-cntrl-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-cntrl-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of CNTRL residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'cntrl 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-digit-in-braced-atpt ()
  "Employ actions of  at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-digit-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-digit-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-digit-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-digit-in-braced-atpt ()
  "Employ actions of BEG at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-digit-in-braced-atpt ()
  "Employ actions of BLOK at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-digit-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-digit-in-braced-atpt ()
  "Employ actions of BRACE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-digit-in-braced-atpt ()
  "Employ actions of BRACKET at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-digit-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-digit-in-braced-atpt ()
  "Employ actions of COMMENT at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-digit-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-digit-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-digit-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-digit-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-digit-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-digit-in-braced-atpt ()
  "Employ actions of END at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-digit-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-digit-in-braced-atpt ()
  "Employ actions of HIDE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-digit-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-digit-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-digit-in-braced-atpt ()
  "Employ actions of KILL at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-digit-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-digit-in-braced-atpt ()
  "Employ actions of LENGTH at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-digit-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-digit-in-braced-atpt ()
  "Employ actions of QUOTE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-digit-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-digit-in-braced-atpt ()
  "Employ actions of SHOW at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-digit-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-digit-in-braced-atpt ()
  "Employ actions of SLASH at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-digit-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-digit-in-braced-atpt ()
  "Employ actions of SORT at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-digit-in-braced-atpt ()
  "Employ actions of TRIM at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-digit-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-digit-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-digit-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-digit-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of DIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-digit-in-bracketed-atpt ()
  "Employ actions of  at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-digit-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-digit-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-digit-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-digit-in-bracketed-atpt ()
  "Employ actions of BEG at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-digit-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-digit-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-digit-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-digit-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-digit-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-digit-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-digit-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-digit-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-digit-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-digit-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-digit-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-digit-in-bracketed-atpt ()
  "Employ actions of END at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-digit-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-digit-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-digit-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-digit-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-digit-in-bracketed-atpt ()
  "Employ actions of KILL at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-digit-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-digit-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-digit-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-digit-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-digit-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-digit-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-digit-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-digit-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-digit-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-digit-in-bracketed-atpt ()
  "Employ actions of SORT at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-digit-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-digit-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-digit-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-digit-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-digit-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of DIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-digit-in-lesserangled-atpt ()
  "Employ actions of  at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-digit-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-digit-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-digit-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-digit-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-digit-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-digit-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-digit-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-digit-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-digit-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-digit-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-digit-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-digit-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-digit-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-digit-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-digit-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-digit-in-lesserangled-atpt ()
  "Employ actions of END at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-digit-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-digit-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-digit-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-digit-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-digit-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-digit-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-digit-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-digit-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-digit-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-digit-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-digit-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-digit-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-digit-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-digit-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-digit-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-digit-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-digit-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-digit-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-digit-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-digit-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of DIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-digit-in-greaterangled-atpt ()
  "Employ actions of  at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-digit-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-digit-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-digit-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-digit-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-digit-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-digit-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-digit-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-digit-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-digit-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-digit-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-digit-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-digit-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-digit-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-digit-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-digit-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-digit-in-greaterangled-atpt ()
  "Employ actions of END at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-digit-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-digit-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-digit-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-digit-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-digit-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-digit-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-digit-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-digit-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-digit-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-digit-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-digit-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-digit-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-digit-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-digit-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-digit-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-digit-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-digit-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-digit-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-digit-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-digit-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of DIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-digit-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of DIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-digit-in-parentized-atpt ()
  "Employ actions of  at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-digit-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-digit-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-digit-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-digit-in-parentized-atpt ()
  "Employ actions of BEG at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-digit-in-parentized-atpt ()
  "Employ actions of BLOK at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-digit-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-digit-in-parentized-atpt ()
  "Employ actions of BRACE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-digit-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-digit-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-digit-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-digit-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-digit-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-digit-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-digit-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-digit-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-digit-in-parentized-atpt ()
  "Employ actions of END at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-digit-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-digit-in-parentized-atpt ()
  "Employ actions of HIDE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-digit-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-digit-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-digit-in-parentized-atpt ()
  "Employ actions of KILL at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-digit-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-digit-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-digit-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-digit-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-digit-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-digit-in-parentized-atpt ()
  "Employ actions of SHOW at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-digit-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-digit-in-parentized-atpt ()
  "Employ actions of SLASH at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-digit-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-digit-in-parentized-atpt ()
  "Employ actions of SORT at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-digit-in-parentized-atpt ()
  "Employ actions of TRIM at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-digit-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-digit-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-digit-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-digit-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of DIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'digit 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-graph-in-braced-atpt ()
  "Employ actions of  at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-graph-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-graph-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-graph-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-graph-in-braced-atpt ()
  "Employ actions of BEG at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-graph-in-braced-atpt ()
  "Employ actions of BLOK at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-graph-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-graph-in-braced-atpt ()
  "Employ actions of BRACE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-graph-in-braced-atpt ()
  "Employ actions of BRACKET at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-graph-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-graph-in-braced-atpt ()
  "Employ actions of COMMENT at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-graph-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-graph-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-graph-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-graph-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-graph-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-graph-in-braced-atpt ()
  "Employ actions of END at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-graph-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-graph-in-braced-atpt ()
  "Employ actions of HIDE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-graph-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-graph-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-graph-in-braced-atpt ()
  "Employ actions of KILL at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-graph-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-graph-in-braced-atpt ()
  "Employ actions of LENGTH at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-graph-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-graph-in-braced-atpt ()
  "Employ actions of QUOTE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-graph-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-graph-in-braced-atpt ()
  "Employ actions of SHOW at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-graph-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-graph-in-braced-atpt ()
  "Employ actions of SLASH at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-graph-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-graph-in-braced-atpt ()
  "Employ actions of SORT at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-graph-in-braced-atpt ()
  "Employ actions of TRIM at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-graph-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-graph-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-graph-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-graph-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of GRAPH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-graph-in-bracketed-atpt ()
  "Employ actions of  at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-graph-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-graph-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-graph-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-graph-in-bracketed-atpt ()
  "Employ actions of BEG at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-graph-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-graph-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-graph-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-graph-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-graph-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-graph-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-graph-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-graph-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-graph-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-graph-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-graph-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-graph-in-bracketed-atpt ()
  "Employ actions of END at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-graph-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-graph-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-graph-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-graph-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-graph-in-bracketed-atpt ()
  "Employ actions of KILL at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-graph-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-graph-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-graph-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-graph-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-graph-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-graph-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-graph-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-graph-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-graph-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-graph-in-bracketed-atpt ()
  "Employ actions of SORT at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-graph-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-graph-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-graph-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-graph-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-graph-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of GRAPH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-graph-in-lesserangled-atpt ()
  "Employ actions of  at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-graph-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-graph-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-graph-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-graph-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-graph-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-graph-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-graph-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-graph-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-graph-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-graph-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-graph-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-graph-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-graph-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-graph-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-graph-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-graph-in-lesserangled-atpt ()
  "Employ actions of END at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-graph-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-graph-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-graph-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-graph-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-graph-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-graph-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-graph-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-graph-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-graph-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-graph-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-graph-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-graph-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-graph-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-graph-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-graph-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-graph-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-graph-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-graph-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-graph-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-graph-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of GRAPH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-graph-in-greaterangled-atpt ()
  "Employ actions of  at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-graph-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-graph-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-graph-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-graph-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-graph-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-graph-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-graph-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-graph-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-graph-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-graph-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-graph-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-graph-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-graph-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-graph-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-graph-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-graph-in-greaterangled-atpt ()
  "Employ actions of END at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-graph-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-graph-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-graph-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-graph-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-graph-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-graph-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-graph-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-graph-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-graph-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-graph-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-graph-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-graph-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-graph-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-graph-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-graph-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-graph-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-graph-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-graph-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-graph-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-graph-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of GRAPH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-graph-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of GRAPH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-graph-in-parentized-atpt ()
  "Employ actions of  at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-graph-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-graph-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-graph-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-graph-in-parentized-atpt ()
  "Employ actions of BEG at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-graph-in-parentized-atpt ()
  "Employ actions of BLOK at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-graph-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-graph-in-parentized-atpt ()
  "Employ actions of BRACE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-graph-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-graph-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-graph-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-graph-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-graph-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-graph-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-graph-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-graph-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-graph-in-parentized-atpt ()
  "Employ actions of END at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-graph-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-graph-in-parentized-atpt ()
  "Employ actions of HIDE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-graph-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-graph-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-graph-in-parentized-atpt ()
  "Employ actions of KILL at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-graph-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-graph-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-graph-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-graph-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-graph-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-graph-in-parentized-atpt ()
  "Employ actions of SHOW at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-graph-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-graph-in-parentized-atpt ()
  "Employ actions of SLASH at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-graph-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-graph-in-parentized-atpt ()
  "Employ actions of SORT at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-graph-in-parentized-atpt ()
  "Employ actions of TRIM at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-graph-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-graph-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-graph-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-graph-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of GRAPH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'graph 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-lower-in-braced-atpt ()
  "Employ actions of  at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-lower-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-lower-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-lower-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-lower-in-braced-atpt ()
  "Employ actions of BEG at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-lower-in-braced-atpt ()
  "Employ actions of BLOK at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-lower-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-lower-in-braced-atpt ()
  "Employ actions of BRACE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-lower-in-braced-atpt ()
  "Employ actions of BRACKET at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-lower-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-lower-in-braced-atpt ()
  "Employ actions of COMMENT at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-lower-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-lower-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-lower-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-lower-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-lower-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-lower-in-braced-atpt ()
  "Employ actions of END at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-lower-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-lower-in-braced-atpt ()
  "Employ actions of HIDE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-lower-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-lower-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-lower-in-braced-atpt ()
  "Employ actions of KILL at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-lower-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-lower-in-braced-atpt ()
  "Employ actions of LENGTH at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-lower-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-lower-in-braced-atpt ()
  "Employ actions of QUOTE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-lower-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-lower-in-braced-atpt ()
  "Employ actions of SHOW at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-lower-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-lower-in-braced-atpt ()
  "Employ actions of SLASH at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-lower-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-lower-in-braced-atpt ()
  "Employ actions of SORT at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-lower-in-braced-atpt ()
  "Employ actions of TRIM at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-lower-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-lower-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-lower-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-lower-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of LOWER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-lower-in-bracketed-atpt ()
  "Employ actions of  at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-lower-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-lower-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-lower-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-lower-in-bracketed-atpt ()
  "Employ actions of BEG at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-lower-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-lower-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-lower-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-lower-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-lower-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-lower-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-lower-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-lower-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-lower-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-lower-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-lower-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-lower-in-bracketed-atpt ()
  "Employ actions of END at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-lower-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-lower-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-lower-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-lower-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-lower-in-bracketed-atpt ()
  "Employ actions of KILL at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-lower-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-lower-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-lower-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-lower-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-lower-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-lower-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-lower-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-lower-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-lower-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-lower-in-bracketed-atpt ()
  "Employ actions of SORT at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-lower-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-lower-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-lower-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-lower-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-lower-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of LOWER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-lower-in-lesserangled-atpt ()
  "Employ actions of  at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-lower-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-lower-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-lower-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-lower-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-lower-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-lower-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-lower-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-lower-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-lower-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-lower-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-lower-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-lower-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-lower-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-lower-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-lower-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-lower-in-lesserangled-atpt ()
  "Employ actions of END at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-lower-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-lower-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-lower-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-lower-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-lower-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-lower-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-lower-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-lower-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-lower-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-lower-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-lower-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-lower-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-lower-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-lower-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-lower-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-lower-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-lower-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-lower-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-lower-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-lower-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of LOWER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-lower-in-greaterangled-atpt ()
  "Employ actions of  at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-lower-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-lower-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-lower-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-lower-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-lower-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-lower-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-lower-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-lower-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-lower-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-lower-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-lower-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-lower-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-lower-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-lower-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-lower-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-lower-in-greaterangled-atpt ()
  "Employ actions of END at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-lower-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-lower-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-lower-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-lower-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-lower-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-lower-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-lower-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-lower-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-lower-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-lower-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-lower-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-lower-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-lower-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-lower-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-lower-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-lower-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-lower-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-lower-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-lower-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-lower-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of LOWER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-lower-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of LOWER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-lower-in-parentized-atpt ()
  "Employ actions of  at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-lower-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-lower-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-lower-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-lower-in-parentized-atpt ()
  "Employ actions of BEG at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-lower-in-parentized-atpt ()
  "Employ actions of BLOK at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-lower-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-lower-in-parentized-atpt ()
  "Employ actions of BRACE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-lower-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-lower-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-lower-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-lower-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-lower-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-lower-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-lower-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-lower-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-lower-in-parentized-atpt ()
  "Employ actions of END at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-lower-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-lower-in-parentized-atpt ()
  "Employ actions of HIDE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-lower-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-lower-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-lower-in-parentized-atpt ()
  "Employ actions of KILL at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-lower-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-lower-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-lower-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-lower-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-lower-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-lower-in-parentized-atpt ()
  "Employ actions of SHOW at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-lower-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-lower-in-parentized-atpt ()
  "Employ actions of SLASH at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-lower-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-lower-in-parentized-atpt ()
  "Employ actions of SORT at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-lower-in-parentized-atpt ()
  "Employ actions of TRIM at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-lower-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-lower-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-lower-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-lower-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of LOWER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lower 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-nonascii-in-braced-atpt ()
  "Employ actions of  at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-nonascii-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-nonascii-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-nonascii-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-nonascii-in-braced-atpt ()
  "Employ actions of BEG at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-nonascii-in-braced-atpt ()
  "Employ actions of BLOK at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-nonascii-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-nonascii-in-braced-atpt ()
  "Employ actions of BRACE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-nonascii-in-braced-atpt ()
  "Employ actions of BRACKET at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-nonascii-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-nonascii-in-braced-atpt ()
  "Employ actions of COMMENT at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-nonascii-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-nonascii-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-nonascii-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-nonascii-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-nonascii-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-nonascii-in-braced-atpt ()
  "Employ actions of END at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-nonascii-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-nonascii-in-braced-atpt ()
  "Employ actions of HIDE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-nonascii-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-nonascii-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-nonascii-in-braced-atpt ()
  "Employ actions of KILL at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-nonascii-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-nonascii-in-braced-atpt ()
  "Employ actions of LENGTH at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-nonascii-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-nonascii-in-braced-atpt ()
  "Employ actions of QUOTE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-nonascii-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-nonascii-in-braced-atpt ()
  "Employ actions of SHOW at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-nonascii-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-nonascii-in-braced-atpt ()
  "Employ actions of SLASH at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-nonascii-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-nonascii-in-braced-atpt ()
  "Employ actions of SORT at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-nonascii-in-braced-atpt ()
  "Employ actions of TRIM at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-nonascii-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-nonascii-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-nonascii-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-nonascii-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of NONASCII residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-nonascii-in-bracketed-atpt ()
  "Employ actions of  at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-nonascii-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-nonascii-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-nonascii-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-nonascii-in-bracketed-atpt ()
  "Employ actions of BEG at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-nonascii-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-nonascii-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-nonascii-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-nonascii-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-nonascii-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-nonascii-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-nonascii-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-nonascii-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-nonascii-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-nonascii-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-nonascii-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-nonascii-in-bracketed-atpt ()
  "Employ actions of END at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-nonascii-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-nonascii-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-nonascii-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-nonascii-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-nonascii-in-bracketed-atpt ()
  "Employ actions of KILL at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-nonascii-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-nonascii-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-nonascii-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-nonascii-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-nonascii-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-nonascii-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-nonascii-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-nonascii-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-nonascii-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-nonascii-in-bracketed-atpt ()
  "Employ actions of SORT at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-nonascii-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-nonascii-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-nonascii-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-nonascii-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-nonascii-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of NONASCII residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-nonascii-in-lesserangled-atpt ()
  "Employ actions of  at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-nonascii-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-nonascii-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-nonascii-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-nonascii-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-nonascii-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-nonascii-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-nonascii-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-nonascii-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-nonascii-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-nonascii-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-nonascii-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-nonascii-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-nonascii-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-nonascii-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-nonascii-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-nonascii-in-lesserangled-atpt ()
  "Employ actions of END at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-nonascii-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-nonascii-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-nonascii-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-nonascii-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-nonascii-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-nonascii-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-nonascii-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-nonascii-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-nonascii-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-nonascii-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-nonascii-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-nonascii-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-nonascii-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-nonascii-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-nonascii-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-nonascii-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-nonascii-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-nonascii-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-nonascii-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-nonascii-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of NONASCII residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-nonascii-in-greaterangled-atpt ()
  "Employ actions of  at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-nonascii-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-nonascii-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-nonascii-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-nonascii-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-nonascii-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-nonascii-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-nonascii-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-nonascii-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-nonascii-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-nonascii-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-nonascii-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-nonascii-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-nonascii-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-nonascii-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-nonascii-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-nonascii-in-greaterangled-atpt ()
  "Employ actions of END at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-nonascii-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-nonascii-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-nonascii-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-nonascii-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-nonascii-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-nonascii-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-nonascii-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-nonascii-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-nonascii-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-nonascii-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-nonascii-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-nonascii-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-nonascii-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-nonascii-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-nonascii-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-nonascii-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-nonascii-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-nonascii-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-nonascii-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-nonascii-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of NONASCII residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-nonascii-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of NONASCII residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-nonascii-in-parentized-atpt ()
  "Employ actions of  at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-nonascii-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-nonascii-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-nonascii-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-nonascii-in-parentized-atpt ()
  "Employ actions of BEG at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-nonascii-in-parentized-atpt ()
  "Employ actions of BLOK at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-nonascii-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-nonascii-in-parentized-atpt ()
  "Employ actions of BRACE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-nonascii-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-nonascii-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-nonascii-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-nonascii-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-nonascii-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-nonascii-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-nonascii-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-nonascii-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-nonascii-in-parentized-atpt ()
  "Employ actions of END at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-nonascii-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-nonascii-in-parentized-atpt ()
  "Employ actions of HIDE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-nonascii-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-nonascii-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-nonascii-in-parentized-atpt ()
  "Employ actions of KILL at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-nonascii-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-nonascii-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-nonascii-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-nonascii-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-nonascii-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-nonascii-in-parentized-atpt ()
  "Employ actions of SHOW at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-nonascii-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-nonascii-in-parentized-atpt ()
  "Employ actions of SLASH at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-nonascii-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-nonascii-in-parentized-atpt ()
  "Employ actions of SORT at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-nonascii-in-parentized-atpt ()
  "Employ actions of TRIM at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-nonascii-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-nonascii-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-nonascii-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-nonascii-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of NONASCII residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'nonascii 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-print-in-braced-atpt ()
  "Employ actions of  at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-print-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-print-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-print-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-print-in-braced-atpt ()
  "Employ actions of BEG at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-print-in-braced-atpt ()
  "Employ actions of BLOK at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-print-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-print-in-braced-atpt ()
  "Employ actions of BRACE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-print-in-braced-atpt ()
  "Employ actions of BRACKET at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-print-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-print-in-braced-atpt ()
  "Employ actions of COMMENT at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-print-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-print-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-print-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-print-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-print-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-print-in-braced-atpt ()
  "Employ actions of END at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-print-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-print-in-braced-atpt ()
  "Employ actions of HIDE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-print-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-print-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-print-in-braced-atpt ()
  "Employ actions of KILL at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-print-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-print-in-braced-atpt ()
  "Employ actions of LENGTH at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-print-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-print-in-braced-atpt ()
  "Employ actions of QUOTE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-print-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-print-in-braced-atpt ()
  "Employ actions of SHOW at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-print-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-print-in-braced-atpt ()
  "Employ actions of SLASH at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-print-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-print-in-braced-atpt ()
  "Employ actions of SORT at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-print-in-braced-atpt ()
  "Employ actions of TRIM at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-print-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-print-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-print-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-print-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of PRINT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-print-in-bracketed-atpt ()
  "Employ actions of  at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-print-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-print-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-print-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-print-in-bracketed-atpt ()
  "Employ actions of BEG at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-print-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-print-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-print-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-print-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-print-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-print-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-print-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-print-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-print-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-print-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-print-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-print-in-bracketed-atpt ()
  "Employ actions of END at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-print-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-print-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-print-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-print-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-print-in-bracketed-atpt ()
  "Employ actions of KILL at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-print-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-print-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-print-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-print-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-print-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-print-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-print-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-print-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-print-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-print-in-bracketed-atpt ()
  "Employ actions of SORT at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-print-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-print-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-print-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-print-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-print-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of PRINT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-print-in-lesserangled-atpt ()
  "Employ actions of  at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-print-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-print-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-print-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-print-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-print-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-print-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-print-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-print-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-print-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-print-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-print-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-print-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-print-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-print-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-print-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-print-in-lesserangled-atpt ()
  "Employ actions of END at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-print-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-print-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-print-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-print-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-print-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-print-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-print-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-print-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-print-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-print-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-print-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-print-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-print-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-print-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-print-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-print-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-print-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-print-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-print-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-print-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of PRINT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-print-in-greaterangled-atpt ()
  "Employ actions of  at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-print-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-print-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-print-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-print-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-print-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-print-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-print-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-print-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-print-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-print-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-print-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-print-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-print-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-print-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-print-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-print-in-greaterangled-atpt ()
  "Employ actions of END at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-print-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-print-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-print-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-print-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-print-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-print-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-print-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-print-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-print-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-print-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-print-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-print-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-print-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-print-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-print-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-print-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-print-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-print-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-print-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-print-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of PRINT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-print-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-print-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-print-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-print-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-print-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-print-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-print-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-print-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-print-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-print-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-print-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-print-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-print-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-print-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-print-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-print-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-print-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-print-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-print-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-print-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-print-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-print-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-print-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-print-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-print-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-print-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-print-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-print-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-print-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-print-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-print-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-print-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-print-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-print-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-print-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-print-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-print-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of PRINT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-print-in-parentized-atpt ()
  "Employ actions of  at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-print-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-print-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-print-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-print-in-parentized-atpt ()
  "Employ actions of BEG at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-print-in-parentized-atpt ()
  "Employ actions of BLOK at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-print-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-print-in-parentized-atpt ()
  "Employ actions of BRACE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-print-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-print-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-print-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-print-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-print-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-print-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-print-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-print-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-print-in-parentized-atpt ()
  "Employ actions of END at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-print-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-print-in-parentized-atpt ()
  "Employ actions of HIDE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-print-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-print-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-print-in-parentized-atpt ()
  "Employ actions of KILL at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-print-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-print-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-print-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-print-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-print-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-print-in-parentized-atpt ()
  "Employ actions of SHOW at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-print-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-print-in-parentized-atpt ()
  "Employ actions of SLASH at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-print-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-print-in-parentized-atpt ()
  "Employ actions of SORT at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-print-in-parentized-atpt ()
  "Employ actions of TRIM at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-print-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-print-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-print-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-print-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of PRINT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'print 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-punct-in-braced-atpt ()
  "Employ actions of  at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-punct-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-punct-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-punct-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-punct-in-braced-atpt ()
  "Employ actions of BEG at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-punct-in-braced-atpt ()
  "Employ actions of BLOK at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-punct-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-punct-in-braced-atpt ()
  "Employ actions of BRACE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-punct-in-braced-atpt ()
  "Employ actions of BRACKET at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-punct-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-punct-in-braced-atpt ()
  "Employ actions of COMMENT at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-punct-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-punct-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-punct-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-punct-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-punct-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-punct-in-braced-atpt ()
  "Employ actions of END at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-punct-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-punct-in-braced-atpt ()
  "Employ actions of HIDE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-punct-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-punct-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-punct-in-braced-atpt ()
  "Employ actions of KILL at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-punct-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-punct-in-braced-atpt ()
  "Employ actions of LENGTH at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-punct-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-punct-in-braced-atpt ()
  "Employ actions of QUOTE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-punct-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-punct-in-braced-atpt ()
  "Employ actions of SHOW at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-punct-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-punct-in-braced-atpt ()
  "Employ actions of SLASH at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-punct-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-punct-in-braced-atpt ()
  "Employ actions of SORT at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-punct-in-braced-atpt ()
  "Employ actions of TRIM at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-punct-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-punct-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-punct-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-punct-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of PUNCT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-punct-in-bracketed-atpt ()
  "Employ actions of  at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-punct-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-punct-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-punct-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-punct-in-bracketed-atpt ()
  "Employ actions of BEG at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-punct-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-punct-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-punct-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-punct-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-punct-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-punct-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-punct-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-punct-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-punct-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-punct-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-punct-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-punct-in-bracketed-atpt ()
  "Employ actions of END at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-punct-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-punct-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-punct-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-punct-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-punct-in-bracketed-atpt ()
  "Employ actions of KILL at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-punct-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-punct-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-punct-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-punct-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-punct-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-punct-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-punct-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-punct-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-punct-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-punct-in-bracketed-atpt ()
  "Employ actions of SORT at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-punct-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-punct-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-punct-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-punct-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-punct-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of PUNCT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-punct-in-lesserangled-atpt ()
  "Employ actions of  at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-punct-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-punct-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-punct-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-punct-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-punct-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-punct-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-punct-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-punct-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-punct-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-punct-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-punct-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-punct-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-punct-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-punct-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-punct-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-punct-in-lesserangled-atpt ()
  "Employ actions of END at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-punct-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-punct-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-punct-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-punct-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-punct-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-punct-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-punct-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-punct-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-punct-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-punct-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-punct-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-punct-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-punct-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-punct-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-punct-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-punct-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-punct-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-punct-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-punct-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-punct-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of PUNCT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-punct-in-greaterangled-atpt ()
  "Employ actions of  at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-punct-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-punct-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-punct-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-punct-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-punct-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-punct-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-punct-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-punct-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-punct-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-punct-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-punct-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-punct-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-punct-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-punct-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-punct-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-punct-in-greaterangled-atpt ()
  "Employ actions of END at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-punct-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-punct-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-punct-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-punct-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-punct-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-punct-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-punct-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-punct-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-punct-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-punct-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-punct-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-punct-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-punct-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-punct-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-punct-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-punct-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-punct-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-punct-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-punct-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-punct-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of PUNCT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-punct-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of PUNCT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-punct-in-parentized-atpt ()
  "Employ actions of  at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-punct-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-punct-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-punct-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-punct-in-parentized-atpt ()
  "Employ actions of BEG at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-punct-in-parentized-atpt ()
  "Employ actions of BLOK at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-punct-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-punct-in-parentized-atpt ()
  "Employ actions of BRACE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-punct-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-punct-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-punct-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-punct-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-punct-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-punct-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-punct-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-punct-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-punct-in-parentized-atpt ()
  "Employ actions of END at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-punct-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-punct-in-parentized-atpt ()
  "Employ actions of HIDE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-punct-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-punct-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-punct-in-parentized-atpt ()
  "Employ actions of KILL at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-punct-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-punct-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-punct-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-punct-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-punct-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-punct-in-parentized-atpt ()
  "Employ actions of SHOW at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-punct-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-punct-in-parentized-atpt ()
  "Employ actions of SLASH at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-punct-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-punct-in-parentized-atpt ()
  "Employ actions of SORT at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-punct-in-parentized-atpt ()
  "Employ actions of TRIM at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-punct-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-punct-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-punct-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-punct-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of PUNCT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'punct 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-space-in-braced-atpt ()
  "Employ actions of  at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-space-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-space-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-space-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-space-in-braced-atpt ()
  "Employ actions of BEG at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-space-in-braced-atpt ()
  "Employ actions of BLOK at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-space-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-space-in-braced-atpt ()
  "Employ actions of BRACE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-space-in-braced-atpt ()
  "Employ actions of BRACKET at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-space-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-space-in-braced-atpt ()
  "Employ actions of COMMENT at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-space-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-space-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-space-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-space-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-space-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-space-in-braced-atpt ()
  "Employ actions of END at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-space-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-space-in-braced-atpt ()
  "Employ actions of HIDE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-space-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-space-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-space-in-braced-atpt ()
  "Employ actions of KILL at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-space-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-space-in-braced-atpt ()
  "Employ actions of LENGTH at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-space-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-space-in-braced-atpt ()
  "Employ actions of QUOTE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-space-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-space-in-braced-atpt ()
  "Employ actions of SHOW at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-space-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-space-in-braced-atpt ()
  "Employ actions of SLASH at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-space-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-space-in-braced-atpt ()
  "Employ actions of SORT at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-space-in-braced-atpt ()
  "Employ actions of TRIM at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-space-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-space-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-space-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-space-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of SPACE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-space-in-bracketed-atpt ()
  "Employ actions of  at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-space-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-space-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-space-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-space-in-bracketed-atpt ()
  "Employ actions of BEG at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-space-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-space-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-space-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-space-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-space-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-space-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-space-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-space-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-space-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-space-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-space-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-space-in-bracketed-atpt ()
  "Employ actions of END at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-space-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-space-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-space-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-space-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-space-in-bracketed-atpt ()
  "Employ actions of KILL at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-space-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-space-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-space-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-space-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-space-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-space-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-space-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-space-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-space-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-space-in-bracketed-atpt ()
  "Employ actions of SORT at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-space-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-space-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-space-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-space-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-space-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of SPACE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-space-in-lesserangled-atpt ()
  "Employ actions of  at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-space-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-space-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-space-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-space-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-space-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-space-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-space-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-space-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-space-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-space-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-space-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-space-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-space-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-space-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-space-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-space-in-lesserangled-atpt ()
  "Employ actions of END at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-space-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-space-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-space-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-space-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-space-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-space-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-space-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-space-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-space-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-space-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-space-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-space-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-space-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-space-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-space-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-space-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-space-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-space-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-space-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-space-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of SPACE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-space-in-greaterangled-atpt ()
  "Employ actions of  at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-space-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-space-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-space-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-space-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-space-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-space-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-space-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-space-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-space-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-space-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-space-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-space-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-space-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-space-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-space-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-space-in-greaterangled-atpt ()
  "Employ actions of END at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-space-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-space-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-space-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-space-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-space-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-space-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-space-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-space-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-space-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-space-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-space-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-space-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-space-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-space-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-space-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-space-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-space-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-space-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-space-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-space-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of SPACE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-space-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-space-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-space-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-space-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-space-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-space-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-space-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-space-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-space-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-space-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-space-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-space-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-space-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-space-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-space-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-space-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-space-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-space-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-space-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-space-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-space-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-space-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-space-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-space-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-space-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-space-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-space-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-space-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-space-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-space-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-space-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-space-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-space-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-space-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-space-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-space-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-space-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of SPACE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-space-in-parentized-atpt ()
  "Employ actions of  at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-space-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-space-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-space-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-space-in-parentized-atpt ()
  "Employ actions of BEG at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-space-in-parentized-atpt ()
  "Employ actions of BLOK at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-space-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-space-in-parentized-atpt ()
  "Employ actions of BRACE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-space-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-space-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-space-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-space-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-space-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-space-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-space-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-space-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-space-in-parentized-atpt ()
  "Employ actions of END at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-space-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-space-in-parentized-atpt ()
  "Employ actions of HIDE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-space-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-space-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-space-in-parentized-atpt ()
  "Employ actions of KILL at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-space-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-space-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-space-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-space-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-space-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-space-in-parentized-atpt ()
  "Employ actions of SHOW at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-space-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-space-in-parentized-atpt ()
  "Employ actions of SLASH at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-space-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-space-in-parentized-atpt ()
  "Employ actions of SORT at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-space-in-parentized-atpt ()
  "Employ actions of TRIM at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-space-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-space-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-space-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-space-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of SPACE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'space 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-upper-in-braced-atpt ()
  "Employ actions of  at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-upper-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-upper-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-upper-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-upper-in-braced-atpt ()
  "Employ actions of BEG at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-upper-in-braced-atpt ()
  "Employ actions of BLOK at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-upper-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-upper-in-braced-atpt ()
  "Employ actions of BRACE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-upper-in-braced-atpt ()
  "Employ actions of BRACKET at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-upper-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-upper-in-braced-atpt ()
  "Employ actions of COMMENT at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-upper-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-upper-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-upper-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-upper-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-upper-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-upper-in-braced-atpt ()
  "Employ actions of END at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-upper-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-upper-in-braced-atpt ()
  "Employ actions of HIDE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-upper-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-upper-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-upper-in-braced-atpt ()
  "Employ actions of KILL at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-upper-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-upper-in-braced-atpt ()
  "Employ actions of LENGTH at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-upper-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-upper-in-braced-atpt ()
  "Employ actions of QUOTE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-upper-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-upper-in-braced-atpt ()
  "Employ actions of SHOW at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-upper-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-upper-in-braced-atpt ()
  "Employ actions of SLASH at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-upper-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-upper-in-braced-atpt ()
  "Employ actions of SORT at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-upper-in-braced-atpt ()
  "Employ actions of TRIM at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-upper-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-upper-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-upper-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-upper-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of UPPER residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-upper-in-bracketed-atpt ()
  "Employ actions of  at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-upper-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-upper-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-upper-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-upper-in-bracketed-atpt ()
  "Employ actions of BEG at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-upper-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-upper-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-upper-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-upper-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-upper-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-upper-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-upper-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-upper-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-upper-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-upper-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-upper-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-upper-in-bracketed-atpt ()
  "Employ actions of END at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-upper-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-upper-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-upper-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-upper-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-upper-in-bracketed-atpt ()
  "Employ actions of KILL at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-upper-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-upper-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-upper-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-upper-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-upper-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-upper-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-upper-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-upper-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-upper-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-upper-in-bracketed-atpt ()
  "Employ actions of SORT at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-upper-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-upper-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-upper-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-upper-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-upper-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of UPPER residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-upper-in-lesserangled-atpt ()
  "Employ actions of  at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-upper-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-upper-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-upper-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-upper-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-upper-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-upper-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-upper-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-upper-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-upper-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-upper-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-upper-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-upper-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-upper-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-upper-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-upper-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-upper-in-lesserangled-atpt ()
  "Employ actions of END at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-upper-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-upper-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-upper-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-upper-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-upper-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-upper-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-upper-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-upper-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-upper-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-upper-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-upper-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-upper-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-upper-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-upper-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-upper-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-upper-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-upper-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-upper-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-upper-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-upper-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of UPPER residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-upper-in-greaterangled-atpt ()
  "Employ actions of  at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-upper-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-upper-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-upper-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-upper-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-upper-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-upper-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-upper-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-upper-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-upper-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-upper-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-upper-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-upper-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-upper-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-upper-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-upper-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-upper-in-greaterangled-atpt ()
  "Employ actions of END at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-upper-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-upper-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-upper-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-upper-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-upper-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-upper-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-upper-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-upper-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-upper-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-upper-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-upper-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-upper-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-upper-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-upper-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-upper-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-upper-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-upper-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-upper-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-upper-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-upper-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of UPPER residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-upper-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of UPPER residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-upper-in-parentized-atpt ()
  "Employ actions of  at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-upper-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-upper-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-upper-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-upper-in-parentized-atpt ()
  "Employ actions of BEG at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-upper-in-parentized-atpt ()
  "Employ actions of BLOK at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-upper-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-upper-in-parentized-atpt ()
  "Employ actions of BRACE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-upper-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-upper-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-upper-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-upper-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-upper-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-upper-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-upper-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-upper-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-upper-in-parentized-atpt ()
  "Employ actions of END at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-upper-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-upper-in-parentized-atpt ()
  "Employ actions of HIDE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-upper-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-upper-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-upper-in-parentized-atpt ()
  "Employ actions of KILL at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-upper-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-upper-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-upper-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-upper-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-upper-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-upper-in-parentized-atpt ()
  "Employ actions of SHOW at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-upper-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-upper-in-parentized-atpt ()
  "Employ actions of SLASH at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-upper-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-upper-in-parentized-atpt ()
  "Employ actions of SORT at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-upper-in-parentized-atpt ()
  "Employ actions of TRIM at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-upper-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-upper-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-upper-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-upper-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of UPPER residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'upper 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-xdigit-in-braced-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-xdigit-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-xdigit-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-xdigit-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-xdigit-in-braced-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-xdigit-in-braced-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-xdigit-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-xdigit-in-braced-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-xdigit-in-braced-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-xdigit-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-xdigit-in-braced-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-xdigit-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-xdigit-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-xdigit-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-xdigit-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-xdigit-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-xdigit-in-braced-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-xdigit-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-xdigit-in-braced-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-xdigit-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-xdigit-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-xdigit-in-braced-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-xdigit-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-xdigit-in-braced-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-xdigit-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-xdigit-in-braced-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-xdigit-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-xdigit-in-braced-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-xdigit-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-xdigit-in-braced-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-xdigit-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-xdigit-in-braced-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-xdigit-in-braced-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-xdigit-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-xdigit-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-xdigit-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-xdigit-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-xdigit-in-bracketed-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-xdigit-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-xdigit-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-xdigit-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-xdigit-in-bracketed-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-xdigit-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-xdigit-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-xdigit-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-xdigit-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-xdigit-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-xdigit-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-xdigit-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-xdigit-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-xdigit-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-xdigit-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-xdigit-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-xdigit-in-bracketed-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-xdigit-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-xdigit-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-xdigit-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-xdigit-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-xdigit-in-bracketed-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-xdigit-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-xdigit-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-xdigit-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-xdigit-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-xdigit-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-xdigit-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-xdigit-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-xdigit-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-xdigit-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-xdigit-in-bracketed-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-xdigit-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-xdigit-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-xdigit-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-xdigit-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-xdigit-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-xdigit-in-lesserangled-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-xdigit-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-xdigit-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-xdigit-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-xdigit-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-xdigit-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-xdigit-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-xdigit-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-xdigit-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-xdigit-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-xdigit-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-xdigit-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-xdigit-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-xdigit-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-xdigit-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-xdigit-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-xdigit-in-lesserangled-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-xdigit-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-xdigit-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-xdigit-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-xdigit-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-xdigit-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-xdigit-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-xdigit-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-xdigit-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-xdigit-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-xdigit-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-xdigit-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-xdigit-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-xdigit-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-xdigit-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-xdigit-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-xdigit-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-xdigit-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-xdigit-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-xdigit-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-xdigit-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-xdigit-in-greaterangled-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-xdigit-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-xdigit-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-xdigit-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-xdigit-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-xdigit-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-xdigit-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-xdigit-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-xdigit-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-xdigit-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-xdigit-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-xdigit-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-xdigit-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-xdigit-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-xdigit-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-xdigit-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-xdigit-in-greaterangled-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-xdigit-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-xdigit-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-xdigit-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-xdigit-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-xdigit-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-xdigit-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-xdigit-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-xdigit-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-xdigit-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-xdigit-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-xdigit-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-xdigit-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-xdigit-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-xdigit-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-xdigit-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-xdigit-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-xdigit-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-xdigit-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-xdigit-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-xdigit-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-xdigit-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-xdigit-in-parentized-atpt ()
  "Employ actions of  at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-xdigit-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-xdigit-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-xdigit-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-xdigit-in-parentized-atpt ()
  "Employ actions of BEG at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-xdigit-in-parentized-atpt ()
  "Employ actions of BLOK at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-xdigit-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-xdigit-in-parentized-atpt ()
  "Employ actions of BRACE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-xdigit-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-xdigit-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-xdigit-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-xdigit-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-xdigit-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-xdigit-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-xdigit-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-xdigit-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-xdigit-in-parentized-atpt ()
  "Employ actions of END at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-xdigit-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-xdigit-in-parentized-atpt ()
  "Employ actions of HIDE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-xdigit-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-xdigit-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-xdigit-in-parentized-atpt ()
  "Employ actions of KILL at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-xdigit-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-xdigit-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-xdigit-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-xdigit-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-xdigit-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-xdigit-in-parentized-atpt ()
  "Employ actions of SHOW at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-xdigit-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-xdigit-in-parentized-atpt ()
  "Employ actions of SLASH at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-xdigit-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-xdigit-in-parentized-atpt ()
  "Employ actions of SORT at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-xdigit-in-parentized-atpt ()
  "Employ actions of TRIM at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-xdigit-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-xdigit-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-xdigit-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-xdigit-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of XDIGIT residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xdigit 'parentized 'ar-th-whitespace (interactive-p)))

(provide 'ar-thingatpt-classes-in-delimited-list)
;;; ar-thingatpt-classes-in-delimited-list.el ends here

