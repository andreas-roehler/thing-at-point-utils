;;; thingatpt-delimited-list-in-delimited-list.el --- thing-in-thing functions -*- lexical-binding: t; -*-

;; Copyright (C) 2010-2026 Andreas Röhler, unless
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

(defun ar-braced-in-braced-atpt ()
  "Employ actions of  at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-braced-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-braced-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-braced-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-braced-in-braced-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-braced-in-braced-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-braced-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-braced-in-braced-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-braced-in-braced-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-braced-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-braced-in-braced-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-braced-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-braced-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-braced-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-braced-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-braced-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-braced-in-braced-atpt ()
  "Employ actions of END at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-braced-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-braced-in-braced-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-braced-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-braced-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-braced-in-braced-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-braced-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-braced-in-braced-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-braced-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-braced-in-braced-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-braced-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-braced-in-braced-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-braced-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-braced-in-braced-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-braced-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-braced-in-braced-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-braced-in-braced-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-braced-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-braced-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-braced-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-braced-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-braced-in-bracketed-atpt ()
  "Employ actions of  at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-braced-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-braced-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-braced-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-braced-in-bracketed-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-braced-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-braced-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-braced-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-braced-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-braced-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-braced-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-braced-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-braced-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-braced-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-braced-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-braced-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-braced-in-bracketed-atpt ()
  "Employ actions of END at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-braced-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-braced-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-braced-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-braced-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-braced-in-bracketed-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-braced-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-braced-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-braced-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-braced-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-braced-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-braced-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-braced-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-braced-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-braced-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-braced-in-bracketed-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-braced-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-braced-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-braced-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-braced-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-braced-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-braced-in-lesserangled-atpt ()
  "Employ actions of  at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-braced-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-braced-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-braced-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-braced-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-braced-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-braced-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-braced-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-braced-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-braced-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-braced-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-braced-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-braced-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-braced-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-braced-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-braced-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-braced-in-lesserangled-atpt ()
  "Employ actions of END at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-braced-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-braced-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-braced-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-braced-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-braced-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-braced-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-braced-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-braced-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-braced-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-braced-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-braced-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-braced-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-braced-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-braced-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-braced-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-braced-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-braced-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-braced-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-braced-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-braced-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-braced-in-greaterangled-atpt ()
  "Employ actions of  at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-braced-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-braced-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-braced-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-braced-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-braced-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-braced-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-braced-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-braced-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-braced-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-braced-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-braced-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-braced-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-braced-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-braced-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-braced-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-braced-in-greaterangled-atpt ()
  "Employ actions of END at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-braced-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-braced-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-braced-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-braced-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-braced-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-braced-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-braced-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-braced-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-braced-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-braced-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-braced-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-braced-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-braced-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-braced-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-braced-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-braced-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-braced-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-braced-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-braced-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-braced-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-braced-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-braced-in-parentized-atpt ()
  "Employ actions of  at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-braced-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-braced-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-braced-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-braced-in-parentized-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-braced-in-parentized-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-braced-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-braced-in-parentized-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-braced-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-braced-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-braced-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-braced-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-braced-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-braced-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-braced-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-braced-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-braced-in-parentized-atpt ()
  "Employ actions of END at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-braced-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-braced-in-parentized-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-braced-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-braced-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-braced-in-parentized-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-braced-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-braced-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-braced-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-braced-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-braced-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-braced-in-parentized-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-braced-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-braced-in-parentized-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-braced-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-braced-in-parentized-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-braced-in-parentized-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-braced-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-braced-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-braced-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-braced-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-bracketed-in-braced-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-bracketed-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-bracketed-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-bracketed-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-bracketed-in-braced-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-bracketed-in-braced-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-bracketed-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-bracketed-in-braced-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-bracketed-in-braced-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-bracketed-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-bracketed-in-braced-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-bracketed-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-bracketed-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-bracketed-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-bracketed-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-bracketed-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-bracketed-in-braced-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-bracketed-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-bracketed-in-braced-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-bracketed-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-bracketed-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-bracketed-in-braced-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-bracketed-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-bracketed-in-braced-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-bracketed-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-bracketed-in-braced-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-bracketed-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-bracketed-in-braced-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-bracketed-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-bracketed-in-braced-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-bracketed-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-bracketed-in-braced-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-bracketed-in-braced-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-bracketed-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-bracketed-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-bracketed-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-bracketed-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-bracketed-in-bracketed-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-bracketed-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-bracketed-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-bracketed-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-bracketed-in-bracketed-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-bracketed-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-bracketed-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-bracketed-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-bracketed-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-bracketed-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-bracketed-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-bracketed-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-bracketed-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-bracketed-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-bracketed-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-bracketed-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-bracketed-in-bracketed-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-bracketed-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-bracketed-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-bracketed-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-bracketed-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-bracketed-in-bracketed-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-bracketed-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-bracketed-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-bracketed-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-bracketed-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-bracketed-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-bracketed-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-bracketed-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-bracketed-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-bracketed-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-bracketed-in-bracketed-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-bracketed-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-bracketed-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-bracketed-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-bracketed-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-bracketed-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-bracketed-in-lesserangled-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-bracketed-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-bracketed-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-bracketed-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-bracketed-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-bracketed-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-bracketed-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-bracketed-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-bracketed-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-bracketed-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-bracketed-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-bracketed-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-bracketed-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-bracketed-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-bracketed-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-bracketed-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-bracketed-in-lesserangled-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-bracketed-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-bracketed-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-bracketed-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-bracketed-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-bracketed-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-bracketed-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-bracketed-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-bracketed-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-bracketed-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-bracketed-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-bracketed-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-bracketed-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-bracketed-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-bracketed-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-bracketed-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-bracketed-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-bracketed-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-bracketed-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-bracketed-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-bracketed-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-bracketed-in-greaterangled-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-bracketed-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-bracketed-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-bracketed-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-bracketed-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-bracketed-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-bracketed-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-bracketed-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-bracketed-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-bracketed-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-bracketed-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-bracketed-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-bracketed-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-bracketed-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-bracketed-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-bracketed-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-bracketed-in-greaterangled-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-bracketed-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-bracketed-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-bracketed-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-bracketed-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-bracketed-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-bracketed-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-bracketed-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-bracketed-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-bracketed-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-bracketed-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-bracketed-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-bracketed-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-bracketed-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-bracketed-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-bracketed-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-bracketed-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-bracketed-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-bracketed-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-bracketed-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-bracketed-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-bracketed-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-bracketed-in-parentized-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-bracketed-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-bracketed-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-bracketed-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-bracketed-in-parentized-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-bracketed-in-parentized-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-bracketed-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-bracketed-in-parentized-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-bracketed-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-bracketed-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-bracketed-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-bracketed-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-bracketed-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-bracketed-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-bracketed-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-bracketed-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-bracketed-in-parentized-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-bracketed-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-bracketed-in-parentized-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-bracketed-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-bracketed-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-bracketed-in-parentized-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-bracketed-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-bracketed-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-bracketed-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-bracketed-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-bracketed-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-bracketed-in-parentized-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-bracketed-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-bracketed-in-parentized-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-bracketed-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-bracketed-in-parentized-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-bracketed-in-parentized-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-bracketed-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-bracketed-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-bracketed-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-bracketed-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-lesserangled-in-braced-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-lesserangled-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-lesserangled-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-lesserangled-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-lesserangled-in-braced-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-lesserangled-in-braced-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-lesserangled-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-lesserangled-in-braced-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-lesserangled-in-braced-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-lesserangled-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-lesserangled-in-braced-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-lesserangled-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-lesserangled-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-lesserangled-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-lesserangled-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-lesserangled-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-lesserangled-in-braced-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-lesserangled-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-lesserangled-in-braced-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-lesserangled-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-lesserangled-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-lesserangled-in-braced-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-lesserangled-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-lesserangled-in-braced-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-lesserangled-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-lesserangled-in-braced-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-lesserangled-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-lesserangled-in-braced-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-lesserangled-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-lesserangled-in-braced-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-lesserangled-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-lesserangled-in-braced-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-lesserangled-in-braced-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-lesserangled-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-lesserangled-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-lesserangled-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-lesserangled-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-lesserangled-in-bracketed-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-lesserangled-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-lesserangled-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-lesserangled-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-lesserangled-in-bracketed-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-lesserangled-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-lesserangled-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-lesserangled-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-lesserangled-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-lesserangled-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-lesserangled-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-lesserangled-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-lesserangled-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-lesserangled-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-lesserangled-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-lesserangled-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-lesserangled-in-bracketed-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-lesserangled-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-lesserangled-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-lesserangled-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-lesserangled-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-lesserangled-in-bracketed-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-lesserangled-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-lesserangled-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-lesserangled-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-lesserangled-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-lesserangled-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-lesserangled-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-lesserangled-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-lesserangled-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-lesserangled-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-lesserangled-in-bracketed-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-lesserangled-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-lesserangled-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-lesserangled-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-lesserangled-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-lesserangled-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-lesserangled-in-lesserangled-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-lesserangled-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-lesserangled-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-lesserangled-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-lesserangled-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-lesserangled-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-lesserangled-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-lesserangled-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-lesserangled-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-lesserangled-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-lesserangled-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-lesserangled-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-lesserangled-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-lesserangled-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-lesserangled-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-lesserangled-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-lesserangled-in-lesserangled-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-lesserangled-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-lesserangled-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-lesserangled-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-lesserangled-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-lesserangled-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-lesserangled-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-lesserangled-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-lesserangled-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-lesserangled-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-lesserangled-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-lesserangled-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-lesserangled-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-lesserangled-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-lesserangled-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-lesserangled-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-lesserangled-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-lesserangled-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-lesserangled-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-lesserangled-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-lesserangled-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-lesserangled-in-greaterangled-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-lesserangled-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-lesserangled-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-lesserangled-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-lesserangled-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-lesserangled-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-lesserangled-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-lesserangled-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-lesserangled-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-lesserangled-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-lesserangled-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-lesserangled-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-lesserangled-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-lesserangled-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-lesserangled-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-lesserangled-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-lesserangled-in-greaterangled-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-lesserangled-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-lesserangled-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-lesserangled-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-lesserangled-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-lesserangled-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-lesserangled-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-lesserangled-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-lesserangled-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-lesserangled-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-lesserangled-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-lesserangled-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-lesserangled-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-lesserangled-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-lesserangled-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-lesserangled-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-lesserangled-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-lesserangled-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-lesserangled-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-lesserangled-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-lesserangled-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-lesserangled-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-lesserangled-in-parentized-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-lesserangled-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-lesserangled-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-lesserangled-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-lesserangled-in-parentized-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-lesserangled-in-parentized-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-lesserangled-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-lesserangled-in-parentized-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-lesserangled-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-lesserangled-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-lesserangled-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-lesserangled-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-lesserangled-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-lesserangled-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-lesserangled-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-lesserangled-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-lesserangled-in-parentized-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-lesserangled-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-lesserangled-in-parentized-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-lesserangled-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-lesserangled-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-lesserangled-in-parentized-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-lesserangled-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-lesserangled-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-lesserangled-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-lesserangled-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-lesserangled-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-lesserangled-in-parentized-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-lesserangled-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-lesserangled-in-parentized-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-lesserangled-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-lesserangled-in-parentized-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-lesserangled-in-parentized-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-lesserangled-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-lesserangled-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-lesserangled-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-lesserangled-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-greaterangled-in-braced-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-greaterangled-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-greaterangled-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-greaterangled-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-greaterangled-in-braced-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-greaterangled-in-braced-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-greaterangled-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-greaterangled-in-braced-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-greaterangled-in-braced-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-greaterangled-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-greaterangled-in-braced-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-greaterangled-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-greaterangled-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-greaterangled-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-greaterangled-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-greaterangled-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-greaterangled-in-braced-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-greaterangled-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-greaterangled-in-braced-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-greaterangled-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-greaterangled-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-greaterangled-in-braced-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-greaterangled-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-greaterangled-in-braced-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-greaterangled-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-greaterangled-in-braced-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-greaterangled-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-greaterangled-in-braced-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-greaterangled-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-greaterangled-in-braced-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-greaterangled-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-greaterangled-in-braced-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-greaterangled-in-braced-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-greaterangled-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-greaterangled-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-greaterangled-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-greaterangled-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-greaterangled-in-bracketed-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-greaterangled-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-greaterangled-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-greaterangled-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-greaterangled-in-bracketed-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-greaterangled-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-greaterangled-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-greaterangled-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-greaterangled-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-greaterangled-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-greaterangled-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-greaterangled-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-greaterangled-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-greaterangled-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-greaterangled-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-greaterangled-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-greaterangled-in-bracketed-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-greaterangled-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-greaterangled-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-greaterangled-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-greaterangled-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-greaterangled-in-bracketed-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-greaterangled-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-greaterangled-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-greaterangled-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-greaterangled-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-greaterangled-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-greaterangled-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-greaterangled-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-greaterangled-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-greaterangled-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-greaterangled-in-bracketed-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-greaterangled-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-greaterangled-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-greaterangled-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-greaterangled-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-greaterangled-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-greaterangled-in-lesserangled-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-greaterangled-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-greaterangled-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-greaterangled-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-greaterangled-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-greaterangled-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-greaterangled-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-greaterangled-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-greaterangled-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-greaterangled-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-greaterangled-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-greaterangled-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-greaterangled-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-greaterangled-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-greaterangled-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-greaterangled-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-greaterangled-in-lesserangled-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-greaterangled-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-greaterangled-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-greaterangled-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-greaterangled-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-greaterangled-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-greaterangled-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-greaterangled-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-greaterangled-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-greaterangled-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-greaterangled-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-greaterangled-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-greaterangled-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-greaterangled-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-greaterangled-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-greaterangled-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-greaterangled-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-greaterangled-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-greaterangled-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-greaterangled-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-greaterangled-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-greaterangled-in-greaterangled-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-greaterangled-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-greaterangled-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-greaterangled-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-greaterangled-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-greaterangled-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-greaterangled-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-greaterangled-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-greaterangled-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-greaterangled-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-greaterangled-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-greaterangled-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-greaterangled-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-greaterangled-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-greaterangled-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-greaterangled-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-greaterangled-in-greaterangled-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-greaterangled-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-greaterangled-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-greaterangled-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-greaterangled-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-greaterangled-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-greaterangled-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-greaterangled-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-greaterangled-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-greaterangled-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-greaterangled-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-greaterangled-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-greaterangled-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-greaterangled-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-greaterangled-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-greaterangled-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-greaterangled-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-greaterangled-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-greaterangled-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-greaterangled-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-greaterangled-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-greaterangled-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-greaterangled-in-parentized-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-greaterangled-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-greaterangled-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-greaterangled-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-greaterangled-in-parentized-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-greaterangled-in-parentized-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-greaterangled-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-greaterangled-in-parentized-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-greaterangled-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-greaterangled-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-greaterangled-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-greaterangled-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-greaterangled-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-greaterangled-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-greaterangled-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-greaterangled-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-greaterangled-in-parentized-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-greaterangled-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-greaterangled-in-parentized-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-greaterangled-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-greaterangled-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-greaterangled-in-parentized-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-greaterangled-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-greaterangled-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-greaterangled-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-greaterangled-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-greaterangled-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-greaterangled-in-parentized-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-greaterangled-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-greaterangled-in-parentized-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-greaterangled-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-greaterangled-in-parentized-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-greaterangled-in-parentized-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-greaterangled-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-greaterangled-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-greaterangled-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-greaterangled-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-left-right-singlequoted-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-left-right-singlequoted-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-left-right-singlequoted-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-left-right-singlequoted-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-left-right-singlequoted-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-left-right-singlequoted-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'parentized 'ar-th-whitespace (interactive-p)))
 
(defun ar-parentized-in-braced-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th (interactive-p)))
 
(defun ar-greaterangle-parentized-in-braced-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-parentized-in-braced-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-parentized-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-parentized-in-braced-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-beg (interactive-p)))
 
(defun ar-blok-parentized-in-braced-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-parentized-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-parentized-in-braced-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-parentized-in-braced-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-parentized-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-parentized-in-braced-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-parentized-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-parentized-in-braced-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-parentized-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-parentized-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-parentized-in-braced-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-parentized-in-braced-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-end (interactive-p)))
 
(defun ar-escape-parentized-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-escape (interactive-p)))
 
(defun ar-hide-parentized-in-braced-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-parentized-in-braced-atpt ()
  "Employ actions of HIDE-SHOW at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-parentized-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-parentized-in-braced-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-parentized-in-braced-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-parentized-in-braced-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-length (interactive-p)))
 
(defun ar-parentize-parentized-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-parentized-in-braced-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-quote (interactive-p)))
 
(defun ar-separate-parentized-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-separate (interactive-p)))
 
(defun ar-show-parentized-in-braced-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-parentized-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-parentized-in-braced-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-parentized-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-parentized-in-braced-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-sort (interactive-p)))
 
(defun ar-trim-parentized-in-braced-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-parentized-in-braced-atpt ()
  "Employ actions of TRIM-LEFT at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-parentized-in-braced-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-parentized-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-parentized-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'braced 'ar-th-whitespace (interactive-p)))
 
(defun ar-parentized-in-bracketed-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th (interactive-p)))
 
(defun ar-greaterangle-parentized-in-bracketed-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-parentized-in-bracketed-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-parentized-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-parentized-in-bracketed-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-beg (interactive-p)))
 
(defun ar-blok-parentized-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-parentized-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-parentized-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-parentized-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-parentized-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-parentized-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-parentized-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-parentized-in-bracketed-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-parentized-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-parentized-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-parentized-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-parentized-in-bracketed-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-end (interactive-p)))
 
(defun ar-escape-parentized-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-escape (interactive-p)))
 
(defun ar-hide-parentized-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-parentized-in-bracketed-atpt ()
  "Employ actions of HIDE-SHOW at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-parentized-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-parentized-in-bracketed-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-parentized-in-bracketed-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-parentized-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-length (interactive-p)))
 
(defun ar-parentize-parentized-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-parentized-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-quote (interactive-p)))
 
(defun ar-separate-parentized-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-separate (interactive-p)))
 
(defun ar-show-parentized-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-parentized-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-parentized-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-parentized-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-parentized-in-bracketed-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-sort (interactive-p)))
 
(defun ar-trim-parentized-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-parentized-in-bracketed-atpt ()
  "Employ actions of TRIM-LEFT at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-parentized-in-bracketed-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-parentized-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-parentized-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'bracketed 'ar-th-whitespace (interactive-p)))
 
(defun ar-parentized-in-lesserangled-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-parentized-in-lesserangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-parentized-in-lesserangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-parentized-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-parentized-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-parentized-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-parentized-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-parentized-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-parentized-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-parentized-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-parentized-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-parentized-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-parentized-in-lesserangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-parentized-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-parentized-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-parentized-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-parentized-in-lesserangled-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-parentized-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-parentized-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-parentized-in-lesserangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-parentized-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-parentized-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-parentized-in-lesserangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-parentized-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-parentized-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-parentized-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-parentized-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-parentized-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-parentized-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-parentized-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-parentized-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-parentized-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-parentized-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-parentized-in-lesserangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-parentized-in-lesserangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-parentized-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-parentized-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'lesserangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-parentized-in-greaterangled-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th (interactive-p)))
 
(defun ar-greaterangle-parentized-in-greaterangled-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-parentized-in-greaterangled-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-parentized-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-parentized-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-beg (interactive-p)))
 
(defun ar-blok-parentized-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-parentized-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-parentized-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-parentized-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-parentized-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-parentized-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-parentized-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-parentized-in-greaterangled-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-parentized-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-parentized-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-parentized-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-parentized-in-greaterangled-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-end (interactive-p)))
 
(defun ar-escape-parentized-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-escape (interactive-p)))
 
(defun ar-hide-parentized-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-parentized-in-greaterangled-atpt ()
  "Employ actions of HIDE-SHOW at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-parentized-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-parentized-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-parentized-in-greaterangled-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-parentized-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-length (interactive-p)))
 
(defun ar-parentize-parentized-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-parentized-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-quote (interactive-p)))
 
(defun ar-separate-parentized-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-separate (interactive-p)))
 
(defun ar-show-parentized-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-parentized-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-parentized-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-parentized-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-parentized-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-sort (interactive-p)))
 
(defun ar-trim-parentized-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-parentized-in-greaterangled-atpt ()
  "Employ actions of TRIM-LEFT at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-parentized-in-greaterangled-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-parentized-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-parentized-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'greaterangled 'ar-th-whitespace (interactive-p)))
 
(defun ar-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th (interactive-p)))
 
(defun ar-greaterangle-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-beg (interactive-p)))
 
(defun ar-blok-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-end (interactive-p)))
 
(defun ar-escape-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-escape (interactive-p)))
 
(defun ar-hide-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE-SHOW at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-length (interactive-p)))
 
(defun ar-parentize-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-quote (interactive-p)))
 
(defun ar-separate-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-separate (interactive-p)))
 
(defun ar-show-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-sort (interactive-p)))
 
(defun ar-trim-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-LEFT at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-parentized-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))
 
(defun ar-parentized-in-parentized-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th (interactive-p)))
 
(defun ar-greaterangle-parentized-in-parentized-atpt ()
  "Employ actions of GREATER-ANGLE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-greaterangle (interactive-p)))
 
(defun ar-lesserangle-parentized-in-parentized-atpt ()
  "Employ actions of LESSER-ANGLE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-lesserangle (interactive-p)))
 
(defun ar-backslash-parentized-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-backslash (interactive-p)))
 
(defun ar-beg-parentized-in-parentized-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-beg (interactive-p)))
 
(defun ar-blok-parentized-in-parentized-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-blok (interactive-p)))
 
(defun ar-bounds-parentized-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-bounds (interactive-p)))
 
(defun ar-brace-parentized-in-parentized-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-brace (interactive-p)))
 
(defun ar-bracket-parentized-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-bracket (interactive-p)))
 
(defun ar-commatize-parentized-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-commatize (interactive-p)))
 
(defun ar-comment-parentized-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-comment (interactive-p)))
 
(defun ar-dollar-parentized-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-dollar (interactive-p)))
 
(defun ar-double-backslash-parentized-in-parentized-atpt ()
  "Employ actions of DOUBLE-BACKSLASH at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-double-backslash (interactive-p)))
 
(defun ar-doublequote-parentized-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-doublequote (interactive-p)))
 
(defun ar-doubleslash-parentized-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-doubleslash (interactive-p)))
 
(defun ar-doubleslash-paren-parentized-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH-PAREN at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-doubleslash-paren (interactive-p)))
 
(defun ar-end-parentized-in-parentized-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-end (interactive-p)))
 
(defun ar-escape-parentized-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-escape (interactive-p)))
 
(defun ar-hide-parentized-in-parentized-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-hide (interactive-p)))
 
(defun ar-hide-show-parentized-in-parentized-atpt ()
  "Employ actions of HIDE-SHOW at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-hide-show (interactive-p)))
 
(defun ar-hyphen-parentized-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-hyphen (interactive-p)))
 
(defun ar-kill-parentized-in-parentized-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-kill (interactive-p)))
 
(defun ar-left-right-singlequote-parentized-in-parentized-atpt ()
  "Employ actions of LEFT-RIGHT-SINGLEQUOTE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-left-right-singlequote (interactive-p)))
 
(defun ar-length-parentized-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-length (interactive-p)))
 
(defun ar-parentize-parentized-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-parentize (interactive-p)))
 
(defun ar-quote-parentized-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-quote (interactive-p)))
 
(defun ar-separate-parentized-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-separate (interactive-p)))
 
(defun ar-show-parentized-in-parentized-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-show (interactive-p)))
 
(defun ar-singlequote-parentized-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-singlequote (interactive-p)))
 
(defun ar-slash-parentized-in-parentized-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-slash (interactive-p)))
 
(defun ar-slashparen-parentized-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-slashparen (interactive-p)))
 
(defun ar-sort-parentized-in-parentized-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-sort (interactive-p)))
 
(defun ar-trim-parentized-in-parentized-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-trim (interactive-p)))
 
(defun ar-trim-left-parentized-in-parentized-atpt ()
  "Employ actions of TRIM-LEFT at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-trim-left (interactive-p)))
 
(defun ar-trim-right-parentized-in-parentized-atpt ()
  "Employ actions of TRIM-RIGHT at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-trim-right (interactive-p)))
 
(defun ar-underscore-parentized-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-underscore (interactive-p)))
 
(defun ar-whitespace-parentized-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'parentized 'ar-th-whitespace (interactive-p)))

(provide 'ar-thingatpt-delimited-list-in-delimited-list)
;;;thingatpt-delimited-list-in-delimited-list.el ends here

