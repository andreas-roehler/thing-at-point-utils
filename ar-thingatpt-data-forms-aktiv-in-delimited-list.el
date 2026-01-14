;;; ar-thing-data-forms-aktiv-in-delimited-list.el --- thing-in-thing functions -*- lexical-binding: t; -*-
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

(defun ar-begin-end-quote-in-braced-atpt ()
  "Employ actions of  at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th (interactive-p)))

(defun ar-greaterangle-begin-end-quote-in-braced-atpt ()
  "Employ actions of GREATERANGLE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-begin-end-quote-in-braced-atpt ()
  "Employ actions of LESSERANGLE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-begin-end-quote-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-backslash (interactive-p)))

(defun ar-backtick-begin-end-quote-in-braced-atpt ()
  "Employ actions of BACKTICK at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-backtick (interactive-p)))

(defun ar-beg-begin-end-quote-in-braced-atpt ()
  "Employ actions of BEG at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-beg (interactive-p)))

(defun ar-blok-begin-end-quote-in-braced-atpt ()
  "Employ actions of BLOK at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-blok (interactive-p)))

(defun ar-bounds-begin-end-quote-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-bounds (interactive-p)))

(defun ar-brace-begin-end-quote-in-braced-atpt ()
  "Employ actions of BRACE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-brace (interactive-p)))

(defun ar-bracket-begin-end-quote-in-braced-atpt ()
  "Employ actions of BRACKET at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-bracket (interactive-p)))

(defun ar-commatize-begin-end-quote-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-commatize (interactive-p)))

(defun ar-comment-begin-end-quote-in-braced-atpt ()
  "Employ actions of COMMENT at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-comment (interactive-p)))

(defun ar-dollar-begin-end-quote-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-begin-end-quote-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-begin-end-quote-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-begin-end-quote-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-begin-end-quote-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-begin-end-quote-in-braced-atpt ()
  "Employ actions of END at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-end (interactive-p)))

(defun ar-escape-begin-end-quote-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-escape (interactive-p)))

(defun ar-hide-begin-end-quote-in-braced-atpt ()
  "Employ actions of HIDE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-hide (interactive-p)))

(defun ar-hide-show-begin-end-quote-in-braced-atpt ()
  "Employ actions of HIDESHOW at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-begin-end-quote-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-hyphen (interactive-p)))

(defun ar-kill-begin-end-quote-in-braced-atpt ()
  "Employ actions of KILL at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-begin-end-quote-in-braced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-begin-end-quote-in-braced-atpt ()
  "Employ actions of LENGTH at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-length (interactive-p)))

(defun ar-parentize-begin-end-quote-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-parentize (interactive-p)))

(defun ar-quote-begin-end-quote-in-braced-atpt ()
  "Employ actions of QUOTE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-quote (interactive-p)))

(defun ar-separate-begin-end-quote-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-separate (interactive-p)))

(defun ar-show-begin-end-quote-in-braced-atpt ()
  "Employ actions of SHOW at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-show (interactive-p)))

(defun ar-singlequote-begin-end-quote-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-singlequote (interactive-p)))

(defun ar-slash-begin-end-quote-in-braced-atpt ()
  "Employ actions of SLASH at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-slash (interactive-p)))

(defun ar-slashparen-begin-end-quote-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-slashparen (interactive-p)))

(defun ar-sort-begin-end-quote-in-braced-atpt ()
  "Employ actions of SORT at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-sort (interactive-p)))

(defun ar-trim-begin-end-quote-in-braced-atpt ()
  "Employ actions of TRIM at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-trim (interactive-p)))

(defun ar-trim-left-begin-end-quote-in-braced-atpt ()
  "Employ actions of TRIMLEFT at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-begin-end-quote-in-braced-atpt ()
  "Employ actions of TRIMRIGHT at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-begin-end-quote-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-begin-end-quote-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of BEGIN-END-QUOTE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'braced 'ar-th-whitespace (interactive-p)))

(defun ar-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of  at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th (interactive-p)))

(defun ar-greaterangle-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of GREATERANGLE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of LESSERANGLE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-backslash (interactive-p)))

(defun ar-backtick-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of BACKTICK at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-backtick (interactive-p)))

(defun ar-beg-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of BEG at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-beg (interactive-p)))

(defun ar-blok-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-blok (interactive-p)))

(defun ar-bounds-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-bounds (interactive-p)))

(defun ar-brace-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-brace (interactive-p)))

(defun ar-bracket-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-bracket (interactive-p)))

(defun ar-commatize-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-commatize (interactive-p)))

(defun ar-comment-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-comment (interactive-p)))

(defun ar-dollar-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of END at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-end (interactive-p)))

(defun ar-escape-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-escape (interactive-p)))

(defun ar-hide-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-hide (interactive-p)))

(defun ar-hide-show-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of HIDESHOW at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-hyphen (interactive-p)))

(defun ar-kill-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of KILL at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-length (interactive-p)))

(defun ar-parentize-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-parentize (interactive-p)))

(defun ar-quote-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-quote (interactive-p)))

(defun ar-separate-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-separate (interactive-p)))

(defun ar-show-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-show (interactive-p)))

(defun ar-singlequote-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-singlequote (interactive-p)))

(defun ar-slash-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-slash (interactive-p)))

(defun ar-slashparen-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-slashparen (interactive-p)))

(defun ar-sort-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of SORT at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-sort (interactive-p)))

(defun ar-trim-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-trim (interactive-p)))

(defun ar-trim-left-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of TRIMLEFT at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of TRIMRIGHT at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-begin-end-quote-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of BEGIN-END-QUOTE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'bracketed 'ar-th-whitespace (interactive-p)))

(defun ar-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of  at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th (interactive-p)))

(defun ar-greaterangle-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of GREATERANGLE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of LESSERANGLE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of BACKTICK at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-beg (interactive-p)))

(defun ar-blok-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of END at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-end (interactive-p)))

(defun ar-escape-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-escape (interactive-p)))

(defun ar-hide-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of HIDESHOW at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-length (interactive-p)))

(defun ar-parentize-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-quote (interactive-p)))

(defun ar-separate-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-separate (interactive-p)))

(defun ar-show-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-sort (interactive-p)))

(defun ar-trim-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of TRIMLEFT at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-begin-end-quote-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of BEGIN-END-QUOTE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'lesserangled 'ar-th-whitespace (interactive-p)))

(defun ar-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of  at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th (interactive-p)))

(defun ar-greaterangle-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of GREATERANGLE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of LESSERANGLE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of BACKTICK at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-beg (interactive-p)))

(defun ar-blok-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of END at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-end (interactive-p)))

(defun ar-escape-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-escape (interactive-p)))

(defun ar-hide-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of HIDESHOW at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-length (interactive-p)))

(defun ar-parentize-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-quote (interactive-p)))

(defun ar-separate-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-separate (interactive-p)))

(defun ar-show-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-sort (interactive-p)))

(defun ar-trim-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of TRIMLEFT at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-begin-end-quote-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of BEGIN-END-QUOTE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'greaterangled 'ar-th-whitespace (interactive-p)))

(defun ar-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th (interactive-p)))

(defun ar-greaterangle-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-backslash (interactive-p)))

(defun ar-backtick-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-backtick (interactive-p)))

(defun ar-beg-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-beg (interactive-p)))

(defun ar-blok-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-blok (interactive-p)))

(defun ar-bounds-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-bounds (interactive-p)))

(defun ar-brace-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-brace (interactive-p)))

(defun ar-bracket-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-bracket (interactive-p)))

(defun ar-commatize-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-commatize (interactive-p)))

(defun ar-comment-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-comment (interactive-p)))

(defun ar-dollar-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-end (interactive-p)))

(defun ar-escape-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-escape (interactive-p)))

(defun ar-hide-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-hide (interactive-p)))

(defun ar-hide-show-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))

(defun ar-kill-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-length (interactive-p)))

(defun ar-parentize-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-parentize (interactive-p)))

(defun ar-quote-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-quote (interactive-p)))

(defun ar-separate-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-separate (interactive-p)))

(defun ar-show-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-show (interactive-p)))

(defun ar-singlequote-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))

(defun ar-slash-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-slash (interactive-p)))

(defun ar-slashparen-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))

(defun ar-sort-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-sort (interactive-p)))

(defun ar-trim-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-trim (interactive-p)))

(defun ar-trim-left-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-begin-end-quote-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of BEGIN-END-QUOTE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))

(defun ar-begin-end-quote-in-parentized-atpt ()
  "Employ actions of  at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th (interactive-p)))

(defun ar-greaterangle-begin-end-quote-in-parentized-atpt ()
  "Employ actions of GREATERANGLE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-begin-end-quote-in-parentized-atpt ()
  "Employ actions of LESSERANGLE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-begin-end-quote-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-backslash (interactive-p)))

(defun ar-backtick-begin-end-quote-in-parentized-atpt ()
  "Employ actions of BACKTICK at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-backtick (interactive-p)))

(defun ar-beg-begin-end-quote-in-parentized-atpt ()
  "Employ actions of BEG at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-beg (interactive-p)))

(defun ar-blok-begin-end-quote-in-parentized-atpt ()
  "Employ actions of BLOK at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-blok (interactive-p)))

(defun ar-bounds-begin-end-quote-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-bounds (interactive-p)))

(defun ar-brace-begin-end-quote-in-parentized-atpt ()
  "Employ actions of BRACE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-brace (interactive-p)))

(defun ar-bracket-begin-end-quote-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-bracket (interactive-p)))

(defun ar-commatize-begin-end-quote-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-commatize (interactive-p)))

(defun ar-comment-begin-end-quote-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-comment (interactive-p)))

(defun ar-dollar-begin-end-quote-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-begin-end-quote-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-begin-end-quote-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-begin-end-quote-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-begin-end-quote-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-begin-end-quote-in-parentized-atpt ()
  "Employ actions of END at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-end (interactive-p)))

(defun ar-escape-begin-end-quote-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-escape (interactive-p)))

(defun ar-hide-begin-end-quote-in-parentized-atpt ()
  "Employ actions of HIDE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-hide (interactive-p)))

(defun ar-hide-show-begin-end-quote-in-parentized-atpt ()
  "Employ actions of HIDESHOW at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-begin-end-quote-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-hyphen (interactive-p)))

(defun ar-kill-begin-end-quote-in-parentized-atpt ()
  "Employ actions of KILL at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-begin-end-quote-in-parentized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-begin-end-quote-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-length (interactive-p)))

(defun ar-parentize-begin-end-quote-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-parentize (interactive-p)))

(defun ar-quote-begin-end-quote-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-quote (interactive-p)))

(defun ar-separate-begin-end-quote-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-separate (interactive-p)))

(defun ar-show-begin-end-quote-in-parentized-atpt ()
  "Employ actions of SHOW at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-show (interactive-p)))

(defun ar-singlequote-begin-end-quote-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-singlequote (interactive-p)))

(defun ar-slash-begin-end-quote-in-parentized-atpt ()
  "Employ actions of SLASH at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-slash (interactive-p)))

(defun ar-slashparen-begin-end-quote-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-slashparen (interactive-p)))

(defun ar-sort-begin-end-quote-in-parentized-atpt ()
  "Employ actions of SORT at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-sort (interactive-p)))

(defun ar-trim-begin-end-quote-in-parentized-atpt ()
  "Employ actions of TRIM at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-trim (interactive-p)))

(defun ar-trim-left-begin-end-quote-in-parentized-atpt ()
  "Employ actions of TRIMLEFT at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-begin-end-quote-in-parentized-atpt ()
  "Employ actions of TRIMRIGHT at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-begin-end-quote-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-begin-end-quote-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of BEGIN-END-QUOTE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'begin-end-quote 'parentized 'ar-th-whitespace (interactive-p)))

(defun ar-blok-in-braced-atpt ()
  "Employ actions of  at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th (interactive-p)))

(defun ar-greaterangle-blok-in-braced-atpt ()
  "Employ actions of GREATERANGLE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-blok-in-braced-atpt ()
  "Employ actions of LESSERANGLE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-blok-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-backslash (interactive-p)))

(defun ar-backtick-blok-in-braced-atpt ()
  "Employ actions of BACKTICK at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-backtick (interactive-p)))

(defun ar-beg-blok-in-braced-atpt ()
  "Employ actions of BEG at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-beg (interactive-p)))

(defun ar-blok-blok-in-braced-atpt ()
  "Employ actions of BLOK at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-blok (interactive-p)))

(defun ar-bounds-blok-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-bounds (interactive-p)))

(defun ar-brace-blok-in-braced-atpt ()
  "Employ actions of BRACE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-brace (interactive-p)))

(defun ar-bracket-blok-in-braced-atpt ()
  "Employ actions of BRACKET at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-bracket (interactive-p)))

(defun ar-commatize-blok-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-commatize (interactive-p)))

(defun ar-comment-blok-in-braced-atpt ()
  "Employ actions of COMMENT at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-comment (interactive-p)))

(defun ar-dollar-blok-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-blok-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-blok-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-blok-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-blok-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-blok-in-braced-atpt ()
  "Employ actions of END at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-end (interactive-p)))

(defun ar-escape-blok-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-escape (interactive-p)))

(defun ar-hide-blok-in-braced-atpt ()
  "Employ actions of HIDE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-hide (interactive-p)))

(defun ar-hide-show-blok-in-braced-atpt ()
  "Employ actions of HIDESHOW at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-blok-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-hyphen (interactive-p)))

(defun ar-kill-blok-in-braced-atpt ()
  "Employ actions of KILL at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-blok-in-braced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-blok-in-braced-atpt ()
  "Employ actions of LENGTH at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-length (interactive-p)))

(defun ar-parentize-blok-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-parentize (interactive-p)))

(defun ar-quote-blok-in-braced-atpt ()
  "Employ actions of QUOTE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-quote (interactive-p)))

(defun ar-separate-blok-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-separate (interactive-p)))

(defun ar-show-blok-in-braced-atpt ()
  "Employ actions of SHOW at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-show (interactive-p)))

(defun ar-singlequote-blok-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-singlequote (interactive-p)))

(defun ar-slash-blok-in-braced-atpt ()
  "Employ actions of SLASH at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-slash (interactive-p)))

(defun ar-slashparen-blok-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-slashparen (interactive-p)))

(defun ar-sort-blok-in-braced-atpt ()
  "Employ actions of SORT at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-sort (interactive-p)))

(defun ar-trim-blok-in-braced-atpt ()
  "Employ actions of TRIM at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-trim (interactive-p)))

(defun ar-trim-left-blok-in-braced-atpt ()
  "Employ actions of TRIMLEFT at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-blok-in-braced-atpt ()
  "Employ actions of TRIMRIGHT at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-blok-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-blok-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of BLOK residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'braced 'ar-th-whitespace (interactive-p)))

(defun ar-blok-in-bracketed-atpt ()
  "Employ actions of  at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th (interactive-p)))

(defun ar-greaterangle-blok-in-bracketed-atpt ()
  "Employ actions of GREATERANGLE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-blok-in-bracketed-atpt ()
  "Employ actions of LESSERANGLE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-blok-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-backslash (interactive-p)))

(defun ar-backtick-blok-in-bracketed-atpt ()
  "Employ actions of BACKTICK at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-backtick (interactive-p)))

(defun ar-beg-blok-in-bracketed-atpt ()
  "Employ actions of BEG at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-beg (interactive-p)))

(defun ar-blok-blok-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-blok (interactive-p)))

(defun ar-bounds-blok-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-bounds (interactive-p)))

(defun ar-brace-blok-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-brace (interactive-p)))

(defun ar-bracket-blok-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-bracket (interactive-p)))

(defun ar-commatize-blok-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-commatize (interactive-p)))

(defun ar-comment-blok-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-comment (interactive-p)))

(defun ar-dollar-blok-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-blok-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-blok-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-blok-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-blok-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-blok-in-bracketed-atpt ()
  "Employ actions of END at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-end (interactive-p)))

(defun ar-escape-blok-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-escape (interactive-p)))

(defun ar-hide-blok-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-hide (interactive-p)))

(defun ar-hide-show-blok-in-bracketed-atpt ()
  "Employ actions of HIDESHOW at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-blok-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-hyphen (interactive-p)))

(defun ar-kill-blok-in-bracketed-atpt ()
  "Employ actions of KILL at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-blok-in-bracketed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-blok-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-length (interactive-p)))

(defun ar-parentize-blok-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-parentize (interactive-p)))

(defun ar-quote-blok-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-quote (interactive-p)))

(defun ar-separate-blok-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-separate (interactive-p)))

(defun ar-show-blok-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-show (interactive-p)))

(defun ar-singlequote-blok-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-singlequote (interactive-p)))

(defun ar-slash-blok-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-slash (interactive-p)))

(defun ar-slashparen-blok-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-slashparen (interactive-p)))

(defun ar-sort-blok-in-bracketed-atpt ()
  "Employ actions of SORT at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-sort (interactive-p)))

(defun ar-trim-blok-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-trim (interactive-p)))

(defun ar-trim-left-blok-in-bracketed-atpt ()
  "Employ actions of TRIMLEFT at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-blok-in-bracketed-atpt ()
  "Employ actions of TRIMRIGHT at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-blok-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-blok-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of BLOK residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'bracketed 'ar-th-whitespace (interactive-p)))

(defun ar-blok-in-lesserangled-atpt ()
  "Employ actions of  at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th (interactive-p)))

(defun ar-greaterangle-blok-in-lesserangled-atpt ()
  "Employ actions of GREATERANGLE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-blok-in-lesserangled-atpt ()
  "Employ actions of LESSERANGLE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-blok-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-blok-in-lesserangled-atpt ()
  "Employ actions of BACKTICK at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-blok-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-beg (interactive-p)))

(defun ar-blok-blok-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-blok-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-blok-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-blok-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-blok-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-blok-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-blok-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-blok-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-blok-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-blok-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-blok-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-blok-in-lesserangled-atpt ()
  "Employ actions of END at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-end (interactive-p)))

(defun ar-escape-blok-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-escape (interactive-p)))

(defun ar-hide-blok-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-blok-in-lesserangled-atpt ()
  "Employ actions of HIDESHOW at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-blok-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-blok-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-blok-in-lesserangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-blok-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-length (interactive-p)))

(defun ar-parentize-blok-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-blok-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-quote (interactive-p)))

(defun ar-separate-blok-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-separate (interactive-p)))

(defun ar-show-blok-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-blok-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-blok-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-blok-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-blok-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-sort (interactive-p)))

(defun ar-trim-blok-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-blok-in-lesserangled-atpt ()
  "Employ actions of TRIMLEFT at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-blok-in-lesserangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-blok-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-blok-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of BLOK residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'lesserangled 'ar-th-whitespace (interactive-p)))

(defun ar-blok-in-greaterangled-atpt ()
  "Employ actions of  at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th (interactive-p)))

(defun ar-greaterangle-blok-in-greaterangled-atpt ()
  "Employ actions of GREATERANGLE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-blok-in-greaterangled-atpt ()
  "Employ actions of LESSERANGLE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-blok-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-blok-in-greaterangled-atpt ()
  "Employ actions of BACKTICK at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-blok-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-beg (interactive-p)))

(defun ar-blok-blok-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-blok-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-blok-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-blok-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-blok-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-blok-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-blok-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-blok-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-blok-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-blok-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-blok-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-blok-in-greaterangled-atpt ()
  "Employ actions of END at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-end (interactive-p)))

(defun ar-escape-blok-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-escape (interactive-p)))

(defun ar-hide-blok-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-blok-in-greaterangled-atpt ()
  "Employ actions of HIDESHOW at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-blok-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-blok-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-blok-in-greaterangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-blok-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-length (interactive-p)))

(defun ar-parentize-blok-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-blok-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-quote (interactive-p)))

(defun ar-separate-blok-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-separate (interactive-p)))

(defun ar-show-blok-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-blok-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-blok-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-blok-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-blok-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-sort (interactive-p)))

(defun ar-trim-blok-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-blok-in-greaterangled-atpt ()
  "Employ actions of TRIMLEFT at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-blok-in-greaterangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-blok-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-blok-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of BLOK residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'greaterangled 'ar-th-whitespace (interactive-p)))

(defun ar-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th (interactive-p)))

(defun ar-greaterangle-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-backslash (interactive-p)))

(defun ar-backtick-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-backtick (interactive-p)))

(defun ar-beg-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-beg (interactive-p)))

(defun ar-blok-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-blok (interactive-p)))

(defun ar-bounds-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-bounds (interactive-p)))

(defun ar-brace-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-brace (interactive-p)))

(defun ar-bracket-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-bracket (interactive-p)))

(defun ar-commatize-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-commatize (interactive-p)))

(defun ar-comment-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-comment (interactive-p)))

(defun ar-dollar-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-end (interactive-p)))

(defun ar-escape-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-escape (interactive-p)))

(defun ar-hide-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-hide (interactive-p)))

(defun ar-hide-show-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))

(defun ar-kill-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-length (interactive-p)))

(defun ar-parentize-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-parentize (interactive-p)))

(defun ar-quote-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-quote (interactive-p)))

(defun ar-separate-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-separate (interactive-p)))

(defun ar-show-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-show (interactive-p)))

(defun ar-singlequote-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))

(defun ar-slash-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-slash (interactive-p)))

(defun ar-slashparen-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))

(defun ar-sort-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-sort (interactive-p)))

(defun ar-trim-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-trim (interactive-p)))

(defun ar-trim-left-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-blok-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of BLOK residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))

(defun ar-blok-in-parentized-atpt ()
  "Employ actions of  at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th (interactive-p)))

(defun ar-greaterangle-blok-in-parentized-atpt ()
  "Employ actions of GREATERANGLE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-blok-in-parentized-atpt ()
  "Employ actions of LESSERANGLE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-blok-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-backslash (interactive-p)))

(defun ar-backtick-blok-in-parentized-atpt ()
  "Employ actions of BACKTICK at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-backtick (interactive-p)))

(defun ar-beg-blok-in-parentized-atpt ()
  "Employ actions of BEG at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-beg (interactive-p)))

(defun ar-blok-blok-in-parentized-atpt ()
  "Employ actions of BLOK at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-blok (interactive-p)))

(defun ar-bounds-blok-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-bounds (interactive-p)))

(defun ar-brace-blok-in-parentized-atpt ()
  "Employ actions of BRACE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-brace (interactive-p)))

(defun ar-bracket-blok-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-bracket (interactive-p)))

(defun ar-commatize-blok-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-commatize (interactive-p)))

(defun ar-comment-blok-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-comment (interactive-p)))

(defun ar-dollar-blok-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-blok-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-blok-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-blok-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-blok-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-blok-in-parentized-atpt ()
  "Employ actions of END at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-end (interactive-p)))

(defun ar-escape-blok-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-escape (interactive-p)))

(defun ar-hide-blok-in-parentized-atpt ()
  "Employ actions of HIDE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-hide (interactive-p)))

(defun ar-hide-show-blok-in-parentized-atpt ()
  "Employ actions of HIDESHOW at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-blok-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-hyphen (interactive-p)))

(defun ar-kill-blok-in-parentized-atpt ()
  "Employ actions of KILL at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-blok-in-parentized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-blok-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-length (interactive-p)))

(defun ar-parentize-blok-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-parentize (interactive-p)))

(defun ar-quote-blok-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-quote (interactive-p)))

(defun ar-separate-blok-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-separate (interactive-p)))

(defun ar-show-blok-in-parentized-atpt ()
  "Employ actions of SHOW at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-show (interactive-p)))

(defun ar-singlequote-blok-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-singlequote (interactive-p)))

(defun ar-slash-blok-in-parentized-atpt ()
  "Employ actions of SLASH at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-slash (interactive-p)))

(defun ar-slashparen-blok-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-slashparen (interactive-p)))

(defun ar-sort-blok-in-parentized-atpt ()
  "Employ actions of SORT at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-sort (interactive-p)))

(defun ar-trim-blok-in-parentized-atpt ()
  "Employ actions of TRIM at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-trim (interactive-p)))

(defun ar-trim-left-blok-in-parentized-atpt ()
  "Employ actions of TRIMLEFT at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-blok-in-parentized-atpt ()
  "Employ actions of TRIMRIGHT at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-blok-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-blok-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of BLOK residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'blok 'parentized 'ar-th-whitespace (interactive-p)))

(defun ar-double-backslash-in-braced-atpt ()
  "Employ actions of  at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th (interactive-p)))

(defun ar-greaterangle-double-backslash-in-braced-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-double-backslash-in-braced-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-double-backslash-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-backslash (interactive-p)))

(defun ar-backtick-double-backslash-in-braced-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-backtick (interactive-p)))

(defun ar-beg-double-backslash-in-braced-atpt ()
  "Employ actions of BEG at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-beg (interactive-p)))

(defun ar-blok-double-backslash-in-braced-atpt ()
  "Employ actions of BLOK at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-blok (interactive-p)))

(defun ar-bounds-double-backslash-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-bounds (interactive-p)))

(defun ar-brace-double-backslash-in-braced-atpt ()
  "Employ actions of BRACE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-brace (interactive-p)))

(defun ar-bracket-double-backslash-in-braced-atpt ()
  "Employ actions of BRACKET at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-bracket (interactive-p)))

(defun ar-commatize-double-backslash-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-commatize (interactive-p)))

(defun ar-comment-double-backslash-in-braced-atpt ()
  "Employ actions of COMMENT at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-comment (interactive-p)))

(defun ar-dollar-double-backslash-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-double-backslash-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-double-backslash-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-double-backslash-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-double-backslash-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-double-backslash-in-braced-atpt ()
  "Employ actions of END at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-end (interactive-p)))

(defun ar-escape-double-backslash-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-escape (interactive-p)))

(defun ar-hide-double-backslash-in-braced-atpt ()
  "Employ actions of HIDE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-hide (interactive-p)))

(defun ar-hide-show-double-backslash-in-braced-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-double-backslash-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-hyphen (interactive-p)))

(defun ar-kill-double-backslash-in-braced-atpt ()
  "Employ actions of KILL at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-double-backslash-in-braced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-double-backslash-in-braced-atpt ()
  "Employ actions of LENGTH at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-length (interactive-p)))

(defun ar-parentize-double-backslash-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-parentize (interactive-p)))

(defun ar-quote-double-backslash-in-braced-atpt ()
  "Employ actions of QUOTE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-quote (interactive-p)))

(defun ar-separate-double-backslash-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-separate (interactive-p)))

(defun ar-show-double-backslash-in-braced-atpt ()
  "Employ actions of SHOW at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-show (interactive-p)))

(defun ar-singlequote-double-backslash-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-singlequote (interactive-p)))

(defun ar-slash-double-backslash-in-braced-atpt ()
  "Employ actions of SLASH at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-slash (interactive-p)))

(defun ar-slashparen-double-backslash-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-slashparen (interactive-p)))

(defun ar-sort-double-backslash-in-braced-atpt ()
  "Employ actions of SORT at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-sort (interactive-p)))

(defun ar-trim-double-backslash-in-braced-atpt ()
  "Employ actions of TRIM at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-trim (interactive-p)))

(defun ar-trim-left-double-backslash-in-braced-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-double-backslash-in-braced-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-double-backslash-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-double-backslash-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLE-BACKSLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'braced 'ar-th-whitespace (interactive-p)))

(defun ar-double-backslash-in-bracketed-atpt ()
  "Employ actions of  at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th (interactive-p)))

(defun ar-greaterangle-double-backslash-in-bracketed-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-double-backslash-in-bracketed-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-double-backslash-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-backslash (interactive-p)))

(defun ar-backtick-double-backslash-in-bracketed-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-backtick (interactive-p)))

(defun ar-beg-double-backslash-in-bracketed-atpt ()
  "Employ actions of BEG at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-beg (interactive-p)))

(defun ar-blok-double-backslash-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-blok (interactive-p)))

(defun ar-bounds-double-backslash-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-bounds (interactive-p)))

(defun ar-brace-double-backslash-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-brace (interactive-p)))

(defun ar-bracket-double-backslash-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-bracket (interactive-p)))

(defun ar-commatize-double-backslash-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-commatize (interactive-p)))

(defun ar-comment-double-backslash-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-comment (interactive-p)))

(defun ar-dollar-double-backslash-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-double-backslash-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-double-backslash-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-double-backslash-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-double-backslash-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-double-backslash-in-bracketed-atpt ()
  "Employ actions of END at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-end (interactive-p)))

(defun ar-escape-double-backslash-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-escape (interactive-p)))

(defun ar-hide-double-backslash-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-hide (interactive-p)))

(defun ar-hide-show-double-backslash-in-bracketed-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-double-backslash-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-hyphen (interactive-p)))

(defun ar-kill-double-backslash-in-bracketed-atpt ()
  "Employ actions of KILL at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-double-backslash-in-bracketed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-double-backslash-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-length (interactive-p)))

(defun ar-parentize-double-backslash-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-parentize (interactive-p)))

(defun ar-quote-double-backslash-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-quote (interactive-p)))

(defun ar-separate-double-backslash-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-separate (interactive-p)))

(defun ar-show-double-backslash-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-show (interactive-p)))

(defun ar-singlequote-double-backslash-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-singlequote (interactive-p)))

(defun ar-slash-double-backslash-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-slash (interactive-p)))

(defun ar-slashparen-double-backslash-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-slashparen (interactive-p)))

(defun ar-sort-double-backslash-in-bracketed-atpt ()
  "Employ actions of SORT at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-sort (interactive-p)))

(defun ar-trim-double-backslash-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-trim (interactive-p)))

(defun ar-trim-left-double-backslash-in-bracketed-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-double-backslash-in-bracketed-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-double-backslash-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-double-backslash-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLE-BACKSLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'bracketed 'ar-th-whitespace (interactive-p)))

(defun ar-double-backslash-in-lesserangled-atpt ()
  "Employ actions of  at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th (interactive-p)))

(defun ar-greaterangle-double-backslash-in-lesserangled-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-double-backslash-in-lesserangled-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-double-backslash-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-double-backslash-in-lesserangled-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-double-backslash-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-beg (interactive-p)))

(defun ar-blok-double-backslash-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-double-backslash-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-double-backslash-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-double-backslash-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-double-backslash-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-double-backslash-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-double-backslash-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-double-backslash-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-double-backslash-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-double-backslash-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-double-backslash-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-double-backslash-in-lesserangled-atpt ()
  "Employ actions of END at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-end (interactive-p)))

(defun ar-escape-double-backslash-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-escape (interactive-p)))

(defun ar-hide-double-backslash-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-double-backslash-in-lesserangled-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-double-backslash-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-double-backslash-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-double-backslash-in-lesserangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-double-backslash-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-length (interactive-p)))

(defun ar-parentize-double-backslash-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-double-backslash-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-quote (interactive-p)))

(defun ar-separate-double-backslash-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-separate (interactive-p)))

(defun ar-show-double-backslash-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-double-backslash-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-double-backslash-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-double-backslash-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-double-backslash-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-sort (interactive-p)))

(defun ar-trim-double-backslash-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-double-backslash-in-lesserangled-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-double-backslash-in-lesserangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-double-backslash-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-double-backslash-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLE-BACKSLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'lesserangled 'ar-th-whitespace (interactive-p)))

(defun ar-double-backslash-in-greaterangled-atpt ()
  "Employ actions of  at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th (interactive-p)))

(defun ar-greaterangle-double-backslash-in-greaterangled-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-double-backslash-in-greaterangled-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-double-backslash-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-double-backslash-in-greaterangled-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-double-backslash-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-beg (interactive-p)))

(defun ar-blok-double-backslash-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-double-backslash-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-double-backslash-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-double-backslash-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-double-backslash-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-double-backslash-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-double-backslash-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-double-backslash-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-double-backslash-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-double-backslash-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-double-backslash-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-double-backslash-in-greaterangled-atpt ()
  "Employ actions of END at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-end (interactive-p)))

(defun ar-escape-double-backslash-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-escape (interactive-p)))

(defun ar-hide-double-backslash-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-double-backslash-in-greaterangled-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-double-backslash-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-double-backslash-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-double-backslash-in-greaterangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-double-backslash-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-length (interactive-p)))

(defun ar-parentize-double-backslash-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-double-backslash-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-quote (interactive-p)))

(defun ar-separate-double-backslash-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-separate (interactive-p)))

(defun ar-show-double-backslash-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-double-backslash-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-double-backslash-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-double-backslash-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-double-backslash-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-sort (interactive-p)))

(defun ar-trim-double-backslash-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-double-backslash-in-greaterangled-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-double-backslash-in-greaterangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-double-backslash-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-double-backslash-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLE-BACKSLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'greaterangled 'ar-th-whitespace (interactive-p)))

(defun ar-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th (interactive-p)))

(defun ar-greaterangle-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-backslash (interactive-p)))

(defun ar-backtick-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-backtick (interactive-p)))

(defun ar-beg-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-beg (interactive-p)))

(defun ar-blok-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-blok (interactive-p)))

(defun ar-bounds-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-bounds (interactive-p)))

(defun ar-brace-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-brace (interactive-p)))

(defun ar-bracket-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-bracket (interactive-p)))

(defun ar-commatize-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-commatize (interactive-p)))

(defun ar-comment-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-comment (interactive-p)))

(defun ar-dollar-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-end (interactive-p)))

(defun ar-escape-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-escape (interactive-p)))

(defun ar-hide-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-hide (interactive-p)))

(defun ar-hide-show-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))

(defun ar-kill-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-length (interactive-p)))

(defun ar-parentize-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-parentize (interactive-p)))

(defun ar-quote-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-quote (interactive-p)))

(defun ar-separate-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-separate (interactive-p)))

(defun ar-show-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-show (interactive-p)))

(defun ar-singlequote-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))

(defun ar-slash-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-slash (interactive-p)))

(defun ar-slashparen-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))

(defun ar-sort-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-sort (interactive-p)))

(defun ar-trim-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-trim (interactive-p)))

(defun ar-trim-left-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-double-backslash-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLE-BACKSLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))

(defun ar-double-backslash-in-parentized-atpt ()
  "Employ actions of  at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th (interactive-p)))

(defun ar-greaterangle-double-backslash-in-parentized-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-double-backslash-in-parentized-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-double-backslash-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-backslash (interactive-p)))

(defun ar-backtick-double-backslash-in-parentized-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-backtick (interactive-p)))

(defun ar-beg-double-backslash-in-parentized-atpt ()
  "Employ actions of BEG at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-beg (interactive-p)))

(defun ar-blok-double-backslash-in-parentized-atpt ()
  "Employ actions of BLOK at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-blok (interactive-p)))

(defun ar-bounds-double-backslash-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-bounds (interactive-p)))

(defun ar-brace-double-backslash-in-parentized-atpt ()
  "Employ actions of BRACE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-brace (interactive-p)))

(defun ar-bracket-double-backslash-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-bracket (interactive-p)))

(defun ar-commatize-double-backslash-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-commatize (interactive-p)))

(defun ar-comment-double-backslash-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-comment (interactive-p)))

(defun ar-dollar-double-backslash-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-double-backslash-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-double-backslash-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-double-backslash-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-double-backslash-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-double-backslash-in-parentized-atpt ()
  "Employ actions of END at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-end (interactive-p)))

(defun ar-escape-double-backslash-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-escape (interactive-p)))

(defun ar-hide-double-backslash-in-parentized-atpt ()
  "Employ actions of HIDE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-hide (interactive-p)))

(defun ar-hide-show-double-backslash-in-parentized-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-double-backslash-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-hyphen (interactive-p)))

(defun ar-kill-double-backslash-in-parentized-atpt ()
  "Employ actions of KILL at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-double-backslash-in-parentized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-double-backslash-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-length (interactive-p)))

(defun ar-parentize-double-backslash-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-parentize (interactive-p)))

(defun ar-quote-double-backslash-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-quote (interactive-p)))

(defun ar-separate-double-backslash-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-separate (interactive-p)))

(defun ar-show-double-backslash-in-parentized-atpt ()
  "Employ actions of SHOW at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-show (interactive-p)))

(defun ar-singlequote-double-backslash-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-singlequote (interactive-p)))

(defun ar-slash-double-backslash-in-parentized-atpt ()
  "Employ actions of SLASH at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-slash (interactive-p)))

(defun ar-slashparen-double-backslash-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-slashparen (interactive-p)))

(defun ar-sort-double-backslash-in-parentized-atpt ()
  "Employ actions of SORT at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-sort (interactive-p)))

(defun ar-trim-double-backslash-in-parentized-atpt ()
  "Employ actions of TRIM at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-trim (interactive-p)))

(defun ar-trim-left-double-backslash-in-parentized-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-double-backslash-in-parentized-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-double-backslash-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-double-backslash-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLE-BACKSLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash 'parentized 'ar-th-whitespace (interactive-p)))

(defun ar-doubleslash-in-braced-atpt ()
  "Employ actions of  at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th (interactive-p)))

(defun ar-greaterangle-doubleslash-in-braced-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doubleslash-in-braced-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doubleslash-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doubleslash-in-braced-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-backtick (interactive-p)))

(defun ar-beg-doubleslash-in-braced-atpt ()
  "Employ actions of BEG at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-beg (interactive-p)))

(defun ar-blok-doubleslash-in-braced-atpt ()
  "Employ actions of BLOK at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-blok (interactive-p)))

(defun ar-bounds-doubleslash-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-bounds (interactive-p)))

(defun ar-brace-doubleslash-in-braced-atpt ()
  "Employ actions of BRACE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-brace (interactive-p)))

(defun ar-bracket-doubleslash-in-braced-atpt ()
  "Employ actions of BRACKET at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doubleslash-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-commatize (interactive-p)))

(defun ar-comment-doubleslash-in-braced-atpt ()
  "Employ actions of COMMENT at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-comment (interactive-p)))

(defun ar-dollar-doubleslash-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doubleslash-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doubleslash-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doubleslash-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doubleslash-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doubleslash-in-braced-atpt ()
  "Employ actions of END at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-end (interactive-p)))

(defun ar-escape-doubleslash-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-escape (interactive-p)))

(defun ar-hide-doubleslash-in-braced-atpt ()
  "Employ actions of HIDE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doubleslash-in-braced-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doubleslash-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doubleslash-in-braced-atpt ()
  "Employ actions of KILL at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doubleslash-in-braced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doubleslash-in-braced-atpt ()
  "Employ actions of LENGTH at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-length (interactive-p)))

(defun ar-parentize-doubleslash-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-parentize (interactive-p)))

(defun ar-quote-doubleslash-in-braced-atpt ()
  "Employ actions of QUOTE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-quote (interactive-p)))

(defun ar-separate-doubleslash-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-separate (interactive-p)))

(defun ar-show-doubleslash-in-braced-atpt ()
  "Employ actions of SHOW at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-show (interactive-p)))

(defun ar-singlequote-doubleslash-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doubleslash-in-braced-atpt ()
  "Employ actions of SLASH at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doubleslash-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doubleslash-in-braced-atpt ()
  "Employ actions of SORT at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-sort (interactive-p)))

(defun ar-trim-doubleslash-in-braced-atpt ()
  "Employ actions of TRIM at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doubleslash-in-braced-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doubleslash-in-braced-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doubleslash-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doubleslash-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLESLASH residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'braced 'ar-th-whitespace (interactive-p)))

(defun ar-doubleslash-in-bracketed-atpt ()
  "Employ actions of  at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th (interactive-p)))

(defun ar-greaterangle-doubleslash-in-bracketed-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doubleslash-in-bracketed-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doubleslash-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doubleslash-in-bracketed-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-backtick (interactive-p)))

(defun ar-beg-doubleslash-in-bracketed-atpt ()
  "Employ actions of BEG at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-beg (interactive-p)))

(defun ar-blok-doubleslash-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-blok (interactive-p)))

(defun ar-bounds-doubleslash-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-bounds (interactive-p)))

(defun ar-brace-doubleslash-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-brace (interactive-p)))

(defun ar-bracket-doubleslash-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doubleslash-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-commatize (interactive-p)))

(defun ar-comment-doubleslash-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-comment (interactive-p)))

(defun ar-dollar-doubleslash-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doubleslash-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doubleslash-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doubleslash-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doubleslash-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doubleslash-in-bracketed-atpt ()
  "Employ actions of END at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-end (interactive-p)))

(defun ar-escape-doubleslash-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-escape (interactive-p)))

(defun ar-hide-doubleslash-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doubleslash-in-bracketed-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doubleslash-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doubleslash-in-bracketed-atpt ()
  "Employ actions of KILL at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doubleslash-in-bracketed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doubleslash-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-length (interactive-p)))

(defun ar-parentize-doubleslash-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-parentize (interactive-p)))

(defun ar-quote-doubleslash-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-quote (interactive-p)))

(defun ar-separate-doubleslash-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-separate (interactive-p)))

(defun ar-show-doubleslash-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-show (interactive-p)))

(defun ar-singlequote-doubleslash-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doubleslash-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doubleslash-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doubleslash-in-bracketed-atpt ()
  "Employ actions of SORT at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-sort (interactive-p)))

(defun ar-trim-doubleslash-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doubleslash-in-bracketed-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doubleslash-in-bracketed-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doubleslash-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doubleslash-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLESLASH residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'bracketed 'ar-th-whitespace (interactive-p)))

(defun ar-doubleslash-in-lesserangled-atpt ()
  "Employ actions of  at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th (interactive-p)))

(defun ar-greaterangle-doubleslash-in-lesserangled-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doubleslash-in-lesserangled-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doubleslash-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doubleslash-in-lesserangled-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-doubleslash-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-beg (interactive-p)))

(defun ar-blok-doubleslash-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-doubleslash-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-doubleslash-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-doubleslash-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doubleslash-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-doubleslash-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-doubleslash-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doubleslash-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doubleslash-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doubleslash-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doubleslash-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doubleslash-in-lesserangled-atpt ()
  "Employ actions of END at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-end (interactive-p)))

(defun ar-escape-doubleslash-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-escape (interactive-p)))

(defun ar-hide-doubleslash-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doubleslash-in-lesserangled-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doubleslash-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doubleslash-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doubleslash-in-lesserangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doubleslash-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-length (interactive-p)))

(defun ar-parentize-doubleslash-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-doubleslash-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-quote (interactive-p)))

(defun ar-separate-doubleslash-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-separate (interactive-p)))

(defun ar-show-doubleslash-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-doubleslash-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doubleslash-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doubleslash-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doubleslash-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-sort (interactive-p)))

(defun ar-trim-doubleslash-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doubleslash-in-lesserangled-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doubleslash-in-lesserangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doubleslash-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doubleslash-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLESLASH residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'lesserangled 'ar-th-whitespace (interactive-p)))

(defun ar-doubleslash-in-greaterangled-atpt ()
  "Employ actions of  at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th (interactive-p)))

(defun ar-greaterangle-doubleslash-in-greaterangled-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doubleslash-in-greaterangled-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doubleslash-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doubleslash-in-greaterangled-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-doubleslash-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-beg (interactive-p)))

(defun ar-blok-doubleslash-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-doubleslash-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-doubleslash-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-doubleslash-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doubleslash-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-doubleslash-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-doubleslash-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doubleslash-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doubleslash-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doubleslash-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doubleslash-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doubleslash-in-greaterangled-atpt ()
  "Employ actions of END at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-end (interactive-p)))

(defun ar-escape-doubleslash-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-escape (interactive-p)))

(defun ar-hide-doubleslash-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doubleslash-in-greaterangled-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doubleslash-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doubleslash-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doubleslash-in-greaterangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doubleslash-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-length (interactive-p)))

(defun ar-parentize-doubleslash-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-doubleslash-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-quote (interactive-p)))

(defun ar-separate-doubleslash-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-separate (interactive-p)))

(defun ar-show-doubleslash-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-doubleslash-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doubleslash-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doubleslash-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doubleslash-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-sort (interactive-p)))

(defun ar-trim-doubleslash-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doubleslash-in-greaterangled-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doubleslash-in-greaterangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doubleslash-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doubleslash-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLESLASH residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'greaterangled 'ar-th-whitespace (interactive-p)))

(defun ar-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th (interactive-p)))

(defun ar-greaterangle-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-backtick (interactive-p)))

(defun ar-beg-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-beg (interactive-p)))

(defun ar-blok-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-blok (interactive-p)))

(defun ar-bounds-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-bounds (interactive-p)))

(defun ar-brace-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-brace (interactive-p)))

(defun ar-bracket-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-commatize (interactive-p)))

(defun ar-comment-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-comment (interactive-p)))

(defun ar-dollar-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-end (interactive-p)))

(defun ar-escape-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-escape (interactive-p)))

(defun ar-hide-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-length (interactive-p)))

(defun ar-parentize-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-parentize (interactive-p)))

(defun ar-quote-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-quote (interactive-p)))

(defun ar-separate-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-separate (interactive-p)))

(defun ar-show-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-show (interactive-p)))

(defun ar-singlequote-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-sort (interactive-p)))

(defun ar-trim-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doubleslash-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLESLASH residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))

(defun ar-doubleslash-in-parentized-atpt ()
  "Employ actions of  at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th (interactive-p)))

(defun ar-greaterangle-doubleslash-in-parentized-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doubleslash-in-parentized-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doubleslash-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doubleslash-in-parentized-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-backtick (interactive-p)))

(defun ar-beg-doubleslash-in-parentized-atpt ()
  "Employ actions of BEG at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-beg (interactive-p)))

(defun ar-blok-doubleslash-in-parentized-atpt ()
  "Employ actions of BLOK at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-blok (interactive-p)))

(defun ar-bounds-doubleslash-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-bounds (interactive-p)))

(defun ar-brace-doubleslash-in-parentized-atpt ()
  "Employ actions of BRACE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-brace (interactive-p)))

(defun ar-bracket-doubleslash-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doubleslash-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-commatize (interactive-p)))

(defun ar-comment-doubleslash-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-comment (interactive-p)))

(defun ar-dollar-doubleslash-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doubleslash-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doubleslash-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doubleslash-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doubleslash-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doubleslash-in-parentized-atpt ()
  "Employ actions of END at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-end (interactive-p)))

(defun ar-escape-doubleslash-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-escape (interactive-p)))

(defun ar-hide-doubleslash-in-parentized-atpt ()
  "Employ actions of HIDE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doubleslash-in-parentized-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doubleslash-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doubleslash-in-parentized-atpt ()
  "Employ actions of KILL at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doubleslash-in-parentized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doubleslash-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-length (interactive-p)))

(defun ar-parentize-doubleslash-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-parentize (interactive-p)))

(defun ar-quote-doubleslash-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-quote (interactive-p)))

(defun ar-separate-doubleslash-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-separate (interactive-p)))

(defun ar-show-doubleslash-in-parentized-atpt ()
  "Employ actions of SHOW at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-show (interactive-p)))

(defun ar-singlequote-doubleslash-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doubleslash-in-parentized-atpt ()
  "Employ actions of SLASH at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doubleslash-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doubleslash-in-parentized-atpt ()
  "Employ actions of SORT at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-sort (interactive-p)))

(defun ar-trim-doubleslash-in-parentized-atpt ()
  "Employ actions of TRIM at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doubleslash-in-parentized-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doubleslash-in-parentized-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doubleslash-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doubleslash-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLESLASH residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'doubleslash 'parentized 'ar-th-whitespace (interactive-p)))

(defun ar-double-backslash-paren-in-braced-atpt ()
  "Employ actions of  at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th (interactive-p)))

(defun ar-greaterangle-double-backslash-paren-in-braced-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-double-backslash-paren-in-braced-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-double-backslash-paren-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-backslash (interactive-p)))

(defun ar-backtick-double-backslash-paren-in-braced-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-backtick (interactive-p)))

(defun ar-beg-double-backslash-paren-in-braced-atpt ()
  "Employ actions of BEG at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-beg (interactive-p)))

(defun ar-blok-double-backslash-paren-in-braced-atpt ()
  "Employ actions of BLOK at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-blok (interactive-p)))

(defun ar-bounds-double-backslash-paren-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-bounds (interactive-p)))

(defun ar-brace-double-backslash-paren-in-braced-atpt ()
  "Employ actions of BRACE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-brace (interactive-p)))

(defun ar-bracket-double-backslash-paren-in-braced-atpt ()
  "Employ actions of BRACKET at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-bracket (interactive-p)))

(defun ar-commatize-double-backslash-paren-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-commatize (interactive-p)))

(defun ar-comment-double-backslash-paren-in-braced-atpt ()
  "Employ actions of COMMENT at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-comment (interactive-p)))

(defun ar-dollar-double-backslash-paren-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-double-backslash-paren-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-double-backslash-paren-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-double-backslash-paren-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-double-backslash-paren-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-double-backslash-paren-in-braced-atpt ()
  "Employ actions of END at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-end (interactive-p)))

(defun ar-escape-double-backslash-paren-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-escape (interactive-p)))

(defun ar-hide-double-backslash-paren-in-braced-atpt ()
  "Employ actions of HIDE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-hide (interactive-p)))

(defun ar-hide-show-double-backslash-paren-in-braced-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-double-backslash-paren-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-hyphen (interactive-p)))

(defun ar-kill-double-backslash-paren-in-braced-atpt ()
  "Employ actions of KILL at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-double-backslash-paren-in-braced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-double-backslash-paren-in-braced-atpt ()
  "Employ actions of LENGTH at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-length (interactive-p)))

(defun ar-parentize-double-backslash-paren-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-parentize (interactive-p)))

(defun ar-quote-double-backslash-paren-in-braced-atpt ()
  "Employ actions of QUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-quote (interactive-p)))

(defun ar-separate-double-backslash-paren-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-separate (interactive-p)))

(defun ar-show-double-backslash-paren-in-braced-atpt ()
  "Employ actions of SHOW at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-show (interactive-p)))

(defun ar-singlequote-double-backslash-paren-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-singlequote (interactive-p)))

(defun ar-slash-double-backslash-paren-in-braced-atpt ()
  "Employ actions of SLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-slash (interactive-p)))

(defun ar-slashparen-double-backslash-paren-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-slashparen (interactive-p)))

(defun ar-sort-double-backslash-paren-in-braced-atpt ()
  "Employ actions of SORT at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-sort (interactive-p)))

(defun ar-trim-double-backslash-paren-in-braced-atpt ()
  "Employ actions of TRIM at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-trim (interactive-p)))

(defun ar-trim-left-double-backslash-paren-in-braced-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-double-backslash-paren-in-braced-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-double-backslash-paren-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-double-backslash-paren-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'braced 'ar-th-whitespace (interactive-p)))

(defun ar-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of  at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th (interactive-p)))

(defun ar-greaterangle-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-backslash (interactive-p)))

(defun ar-backtick-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-backtick (interactive-p)))

(defun ar-beg-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of BEG at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-beg (interactive-p)))

(defun ar-blok-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-blok (interactive-p)))

(defun ar-bounds-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-bounds (interactive-p)))

(defun ar-brace-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-brace (interactive-p)))

(defun ar-bracket-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-bracket (interactive-p)))

(defun ar-commatize-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-commatize (interactive-p)))

(defun ar-comment-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-comment (interactive-p)))

(defun ar-dollar-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of END at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-end (interactive-p)))

(defun ar-escape-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-escape (interactive-p)))

(defun ar-hide-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-hide (interactive-p)))

(defun ar-hide-show-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-hyphen (interactive-p)))

(defun ar-kill-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of KILL at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-length (interactive-p)))

(defun ar-parentize-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-parentize (interactive-p)))

(defun ar-quote-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-quote (interactive-p)))

(defun ar-separate-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-separate (interactive-p)))

(defun ar-show-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-show (interactive-p)))

(defun ar-singlequote-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-singlequote (interactive-p)))

(defun ar-slash-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-slash (interactive-p)))

(defun ar-slashparen-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-slashparen (interactive-p)))

(defun ar-sort-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of SORT at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-sort (interactive-p)))

(defun ar-trim-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-trim (interactive-p)))

(defun ar-trim-left-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-double-backslash-paren-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLE-BACKSLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'bracketed 'ar-th-whitespace (interactive-p)))

(defun ar-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of  at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th (interactive-p)))

(defun ar-greaterangle-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-beg (interactive-p)))

(defun ar-blok-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of END at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-end (interactive-p)))

(defun ar-escape-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-escape (interactive-p)))

(defun ar-hide-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-length (interactive-p)))

(defun ar-parentize-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-quote (interactive-p)))

(defun ar-separate-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-separate (interactive-p)))

(defun ar-show-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-sort (interactive-p)))

(defun ar-trim-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-double-backslash-paren-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLE-BACKSLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'lesserangled 'ar-th-whitespace (interactive-p)))

(defun ar-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of  at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th (interactive-p)))

(defun ar-greaterangle-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-beg (interactive-p)))

(defun ar-blok-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of END at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-end (interactive-p)))

(defun ar-escape-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-escape (interactive-p)))

(defun ar-hide-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-length (interactive-p)))

(defun ar-parentize-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-quote (interactive-p)))

(defun ar-separate-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-separate (interactive-p)))

(defun ar-show-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-sort (interactive-p)))

(defun ar-trim-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-double-backslash-paren-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLE-BACKSLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'greaterangled 'ar-th-whitespace (interactive-p)))

(defun ar-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th (interactive-p)))

(defun ar-greaterangle-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-backslash (interactive-p)))

(defun ar-backtick-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-backtick (interactive-p)))

(defun ar-beg-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-beg (interactive-p)))

(defun ar-blok-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-blok (interactive-p)))

(defun ar-bounds-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-bounds (interactive-p)))

(defun ar-brace-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-brace (interactive-p)))

(defun ar-bracket-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-bracket (interactive-p)))

(defun ar-commatize-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-commatize (interactive-p)))

(defun ar-comment-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-comment (interactive-p)))

(defun ar-dollar-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-end (interactive-p)))

(defun ar-escape-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-escape (interactive-p)))

(defun ar-hide-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-hide (interactive-p)))

(defun ar-hide-show-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))

(defun ar-kill-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-length (interactive-p)))

(defun ar-parentize-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-parentize (interactive-p)))

(defun ar-quote-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-quote (interactive-p)))

(defun ar-separate-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-separate (interactive-p)))

(defun ar-show-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-show (interactive-p)))

(defun ar-singlequote-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))

(defun ar-slash-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-slash (interactive-p)))

(defun ar-slashparen-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))

(defun ar-sort-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-sort (interactive-p)))

(defun ar-trim-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-trim (interactive-p)))

(defun ar-trim-left-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-double-backslash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLE-BACKSLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))

(defun ar-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of  at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th (interactive-p)))

(defun ar-greaterangle-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-backslash (interactive-p)))

(defun ar-backtick-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-backtick (interactive-p)))

(defun ar-beg-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of BEG at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-beg (interactive-p)))

(defun ar-blok-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of BLOK at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-blok (interactive-p)))

(defun ar-bounds-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-bounds (interactive-p)))

(defun ar-brace-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of BRACE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-brace (interactive-p)))

(defun ar-bracket-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-bracket (interactive-p)))

(defun ar-commatize-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-commatize (interactive-p)))

(defun ar-comment-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-comment (interactive-p)))

(defun ar-dollar-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of END at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-end (interactive-p)))

(defun ar-escape-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-escape (interactive-p)))

(defun ar-hide-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of HIDE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-hide (interactive-p)))

(defun ar-hide-show-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-hyphen (interactive-p)))

(defun ar-kill-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of KILL at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-length (interactive-p)))

(defun ar-parentize-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-parentize (interactive-p)))

(defun ar-quote-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-quote (interactive-p)))

(defun ar-separate-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-separate (interactive-p)))

(defun ar-show-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of SHOW at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-show (interactive-p)))

(defun ar-singlequote-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-singlequote (interactive-p)))

(defun ar-slash-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of SLASH at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-slash (interactive-p)))

(defun ar-slashparen-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-slashparen (interactive-p)))

(defun ar-sort-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of SORT at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-sort (interactive-p)))

(defun ar-trim-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of TRIM at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-trim (interactive-p)))

(defun ar-trim-left-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-double-backslash-paren-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLE-BACKSLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'double-backslash-paren 'parentized 'ar-th-whitespace (interactive-p)))

(defun ar-tabledata-in-braced-atpt ()
  "Employ actions of  at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th (interactive-p)))

(defun ar-greaterangle-tabledata-in-braced-atpt ()
  "Employ actions of GREATERANGLE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-tabledata-in-braced-atpt ()
  "Employ actions of LESSERANGLE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-tabledata-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-backslash (interactive-p)))

(defun ar-backtick-tabledata-in-braced-atpt ()
  "Employ actions of BACKTICK at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-backtick (interactive-p)))

(defun ar-beg-tabledata-in-braced-atpt ()
  "Employ actions of BEG at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-beg (interactive-p)))

(defun ar-blok-tabledata-in-braced-atpt ()
  "Employ actions of BLOK at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-blok (interactive-p)))

(defun ar-bounds-tabledata-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-bounds (interactive-p)))

(defun ar-brace-tabledata-in-braced-atpt ()
  "Employ actions of BRACE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-brace (interactive-p)))

(defun ar-bracket-tabledata-in-braced-atpt ()
  "Employ actions of BRACKET at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-bracket (interactive-p)))

(defun ar-commatize-tabledata-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-commatize (interactive-p)))

(defun ar-comment-tabledata-in-braced-atpt ()
  "Employ actions of COMMENT at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-comment (interactive-p)))

(defun ar-dollar-tabledata-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-tabledata-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-tabledata-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-tabledata-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-tabledata-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-tabledata-in-braced-atpt ()
  "Employ actions of END at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-end (interactive-p)))

(defun ar-escape-tabledata-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-escape (interactive-p)))

(defun ar-hide-tabledata-in-braced-atpt ()
  "Employ actions of HIDE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-hide (interactive-p)))

(defun ar-hide-show-tabledata-in-braced-atpt ()
  "Employ actions of HIDESHOW at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-tabledata-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-hyphen (interactive-p)))

(defun ar-kill-tabledata-in-braced-atpt ()
  "Employ actions of KILL at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-tabledata-in-braced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-tabledata-in-braced-atpt ()
  "Employ actions of LENGTH at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-length (interactive-p)))

(defun ar-parentize-tabledata-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-parentize (interactive-p)))

(defun ar-quote-tabledata-in-braced-atpt ()
  "Employ actions of QUOTE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-quote (interactive-p)))

(defun ar-separate-tabledata-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-separate (interactive-p)))

(defun ar-show-tabledata-in-braced-atpt ()
  "Employ actions of SHOW at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-show (interactive-p)))

(defun ar-singlequote-tabledata-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-singlequote (interactive-p)))

(defun ar-slash-tabledata-in-braced-atpt ()
  "Employ actions of SLASH at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-slash (interactive-p)))

(defun ar-slashparen-tabledata-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-slashparen (interactive-p)))

(defun ar-sort-tabledata-in-braced-atpt ()
  "Employ actions of SORT at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-sort (interactive-p)))

(defun ar-trim-tabledata-in-braced-atpt ()
  "Employ actions of TRIM at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-trim (interactive-p)))

(defun ar-trim-left-tabledata-in-braced-atpt ()
  "Employ actions of TRIMLEFT at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-tabledata-in-braced-atpt ()
  "Employ actions of TRIMRIGHT at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-tabledata-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-tabledata-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of TABLEDATA residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'braced 'ar-th-whitespace (interactive-p)))

(defun ar-tabledata-in-bracketed-atpt ()
  "Employ actions of  at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th (interactive-p)))

(defun ar-greaterangle-tabledata-in-bracketed-atpt ()
  "Employ actions of GREATERANGLE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-tabledata-in-bracketed-atpt ()
  "Employ actions of LESSERANGLE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-tabledata-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-backslash (interactive-p)))

(defun ar-backtick-tabledata-in-bracketed-atpt ()
  "Employ actions of BACKTICK at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-backtick (interactive-p)))

(defun ar-beg-tabledata-in-bracketed-atpt ()
  "Employ actions of BEG at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-beg (interactive-p)))

(defun ar-blok-tabledata-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-blok (interactive-p)))

(defun ar-bounds-tabledata-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-bounds (interactive-p)))

(defun ar-brace-tabledata-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-brace (interactive-p)))

(defun ar-bracket-tabledata-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-bracket (interactive-p)))

(defun ar-commatize-tabledata-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-commatize (interactive-p)))

(defun ar-comment-tabledata-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-comment (interactive-p)))

(defun ar-dollar-tabledata-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-tabledata-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-tabledata-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-tabledata-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-tabledata-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-tabledata-in-bracketed-atpt ()
  "Employ actions of END at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-end (interactive-p)))

(defun ar-escape-tabledata-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-escape (interactive-p)))

(defun ar-hide-tabledata-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-hide (interactive-p)))

(defun ar-hide-show-tabledata-in-bracketed-atpt ()
  "Employ actions of HIDESHOW at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-tabledata-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-hyphen (interactive-p)))

(defun ar-kill-tabledata-in-bracketed-atpt ()
  "Employ actions of KILL at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-tabledata-in-bracketed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-tabledata-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-length (interactive-p)))

(defun ar-parentize-tabledata-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-parentize (interactive-p)))

(defun ar-quote-tabledata-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-quote (interactive-p)))

(defun ar-separate-tabledata-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-separate (interactive-p)))

(defun ar-show-tabledata-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-show (interactive-p)))

(defun ar-singlequote-tabledata-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-singlequote (interactive-p)))

(defun ar-slash-tabledata-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-slash (interactive-p)))

(defun ar-slashparen-tabledata-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-slashparen (interactive-p)))

(defun ar-sort-tabledata-in-bracketed-atpt ()
  "Employ actions of SORT at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-sort (interactive-p)))

(defun ar-trim-tabledata-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-trim (interactive-p)))

(defun ar-trim-left-tabledata-in-bracketed-atpt ()
  "Employ actions of TRIMLEFT at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-tabledata-in-bracketed-atpt ()
  "Employ actions of TRIMRIGHT at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-tabledata-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-tabledata-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of TABLEDATA residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'bracketed 'ar-th-whitespace (interactive-p)))

(defun ar-tabledata-in-lesserangled-atpt ()
  "Employ actions of  at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th (interactive-p)))

(defun ar-greaterangle-tabledata-in-lesserangled-atpt ()
  "Employ actions of GREATERANGLE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-tabledata-in-lesserangled-atpt ()
  "Employ actions of LESSERANGLE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-tabledata-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-tabledata-in-lesserangled-atpt ()
  "Employ actions of BACKTICK at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-tabledata-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-beg (interactive-p)))

(defun ar-blok-tabledata-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-tabledata-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-tabledata-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-tabledata-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-tabledata-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-tabledata-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-tabledata-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-tabledata-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-tabledata-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-tabledata-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-tabledata-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-tabledata-in-lesserangled-atpt ()
  "Employ actions of END at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-end (interactive-p)))

(defun ar-escape-tabledata-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-escape (interactive-p)))

(defun ar-hide-tabledata-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-tabledata-in-lesserangled-atpt ()
  "Employ actions of HIDESHOW at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-tabledata-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-tabledata-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-tabledata-in-lesserangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-tabledata-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-length (interactive-p)))

(defun ar-parentize-tabledata-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-tabledata-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-quote (interactive-p)))

(defun ar-separate-tabledata-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-separate (interactive-p)))

(defun ar-show-tabledata-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-tabledata-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-tabledata-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-tabledata-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-tabledata-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-sort (interactive-p)))

(defun ar-trim-tabledata-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-tabledata-in-lesserangled-atpt ()
  "Employ actions of TRIMLEFT at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-tabledata-in-lesserangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-tabledata-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-tabledata-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of TABLEDATA residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'lesserangled 'ar-th-whitespace (interactive-p)))

(defun ar-tabledata-in-greaterangled-atpt ()
  "Employ actions of  at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th (interactive-p)))

(defun ar-greaterangle-tabledata-in-greaterangled-atpt ()
  "Employ actions of GREATERANGLE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-tabledata-in-greaterangled-atpt ()
  "Employ actions of LESSERANGLE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-tabledata-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-tabledata-in-greaterangled-atpt ()
  "Employ actions of BACKTICK at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-tabledata-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-beg (interactive-p)))

(defun ar-blok-tabledata-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-tabledata-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-tabledata-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-tabledata-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-tabledata-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-tabledata-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-tabledata-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-tabledata-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-tabledata-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-tabledata-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-tabledata-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-tabledata-in-greaterangled-atpt ()
  "Employ actions of END at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-end (interactive-p)))

(defun ar-escape-tabledata-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-escape (interactive-p)))

(defun ar-hide-tabledata-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-tabledata-in-greaterangled-atpt ()
  "Employ actions of HIDESHOW at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-tabledata-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-tabledata-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-tabledata-in-greaterangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-tabledata-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-length (interactive-p)))

(defun ar-parentize-tabledata-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-tabledata-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-quote (interactive-p)))

(defun ar-separate-tabledata-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-separate (interactive-p)))

(defun ar-show-tabledata-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-tabledata-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-tabledata-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-tabledata-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-tabledata-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-sort (interactive-p)))

(defun ar-trim-tabledata-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-tabledata-in-greaterangled-atpt ()
  "Employ actions of TRIMLEFT at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-tabledata-in-greaterangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-tabledata-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-tabledata-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of TABLEDATA residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'greaterangled 'ar-th-whitespace (interactive-p)))

(defun ar-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th (interactive-p)))

(defun ar-greaterangle-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-backslash (interactive-p)))

(defun ar-backtick-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-backtick (interactive-p)))

(defun ar-beg-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-beg (interactive-p)))

(defun ar-blok-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-blok (interactive-p)))

(defun ar-bounds-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-bounds (interactive-p)))

(defun ar-brace-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-brace (interactive-p)))

(defun ar-bracket-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-bracket (interactive-p)))

(defun ar-commatize-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-commatize (interactive-p)))

(defun ar-comment-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-comment (interactive-p)))

(defun ar-dollar-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-end (interactive-p)))

(defun ar-escape-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-escape (interactive-p)))

(defun ar-hide-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-hide (interactive-p)))

(defun ar-hide-show-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))

(defun ar-kill-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-length (interactive-p)))

(defun ar-parentize-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-parentize (interactive-p)))

(defun ar-quote-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-quote (interactive-p)))

(defun ar-separate-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-separate (interactive-p)))

(defun ar-show-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-show (interactive-p)))

(defun ar-singlequote-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))

(defun ar-slash-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-slash (interactive-p)))

(defun ar-slashparen-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))

(defun ar-sort-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-sort (interactive-p)))

(defun ar-trim-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-trim (interactive-p)))

(defun ar-trim-left-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-tabledata-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of TABLEDATA residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))

(defun ar-tabledata-in-parentized-atpt ()
  "Employ actions of  at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th (interactive-p)))

(defun ar-greaterangle-tabledata-in-parentized-atpt ()
  "Employ actions of GREATERANGLE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-tabledata-in-parentized-atpt ()
  "Employ actions of LESSERANGLE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-tabledata-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-backslash (interactive-p)))

(defun ar-backtick-tabledata-in-parentized-atpt ()
  "Employ actions of BACKTICK at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-backtick (interactive-p)))

(defun ar-beg-tabledata-in-parentized-atpt ()
  "Employ actions of BEG at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-beg (interactive-p)))

(defun ar-blok-tabledata-in-parentized-atpt ()
  "Employ actions of BLOK at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-blok (interactive-p)))

(defun ar-bounds-tabledata-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-bounds (interactive-p)))

(defun ar-brace-tabledata-in-parentized-atpt ()
  "Employ actions of BRACE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-brace (interactive-p)))

(defun ar-bracket-tabledata-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-bracket (interactive-p)))

(defun ar-commatize-tabledata-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-commatize (interactive-p)))

(defun ar-comment-tabledata-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-comment (interactive-p)))

(defun ar-dollar-tabledata-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-tabledata-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-tabledata-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-tabledata-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-tabledata-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-tabledata-in-parentized-atpt ()
  "Employ actions of END at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-end (interactive-p)))

(defun ar-escape-tabledata-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-escape (interactive-p)))

(defun ar-hide-tabledata-in-parentized-atpt ()
  "Employ actions of HIDE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-hide (interactive-p)))

(defun ar-hide-show-tabledata-in-parentized-atpt ()
  "Employ actions of HIDESHOW at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-tabledata-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-hyphen (interactive-p)))

(defun ar-kill-tabledata-in-parentized-atpt ()
  "Employ actions of KILL at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-tabledata-in-parentized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-tabledata-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-length (interactive-p)))

(defun ar-parentize-tabledata-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-parentize (interactive-p)))

(defun ar-quote-tabledata-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-quote (interactive-p)))

(defun ar-separate-tabledata-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-separate (interactive-p)))

(defun ar-show-tabledata-in-parentized-atpt ()
  "Employ actions of SHOW at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-show (interactive-p)))

(defun ar-singlequote-tabledata-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-singlequote (interactive-p)))

(defun ar-slash-tabledata-in-parentized-atpt ()
  "Employ actions of SLASH at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-slash (interactive-p)))

(defun ar-slashparen-tabledata-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-slashparen (interactive-p)))

(defun ar-sort-tabledata-in-parentized-atpt ()
  "Employ actions of SORT at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-sort (interactive-p)))

(defun ar-trim-tabledata-in-parentized-atpt ()
  "Employ actions of TRIM at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-trim (interactive-p)))

(defun ar-trim-left-tabledata-in-parentized-atpt ()
  "Employ actions of TRIMLEFT at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-tabledata-in-parentized-atpt ()
  "Employ actions of TRIMRIGHT at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-tabledata-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-tabledata-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of TABLEDATA residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'tabledata 'parentized 'ar-th-whitespace (interactive-p)))

(defun ar-slash-paren-in-braced-atpt ()
  "Employ actions of  at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th (interactive-p)))

(defun ar-greaterangle-slash-paren-in-braced-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slash-paren-in-braced-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slash-paren-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slash-paren-in-braced-atpt ()
  "Employ actions of BACKTICK at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-backtick (interactive-p)))

(defun ar-beg-slash-paren-in-braced-atpt ()
  "Employ actions of BEG at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-beg (interactive-p)))

(defun ar-blok-slash-paren-in-braced-atpt ()
  "Employ actions of BLOK at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-blok (interactive-p)))

(defun ar-bounds-slash-paren-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-bounds (interactive-p)))

(defun ar-brace-slash-paren-in-braced-atpt ()
  "Employ actions of BRACE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-brace (interactive-p)))

(defun ar-bracket-slash-paren-in-braced-atpt ()
  "Employ actions of BRACKET at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slash-paren-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-commatize (interactive-p)))

(defun ar-comment-slash-paren-in-braced-atpt ()
  "Employ actions of COMMENT at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-comment (interactive-p)))

(defun ar-dollar-slash-paren-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slash-paren-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slash-paren-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slash-paren-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slash-paren-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slash-paren-in-braced-atpt ()
  "Employ actions of END at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-end (interactive-p)))

(defun ar-escape-slash-paren-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-escape (interactive-p)))

(defun ar-hide-slash-paren-in-braced-atpt ()
  "Employ actions of HIDE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slash-paren-in-braced-atpt ()
  "Employ actions of HIDESHOW at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slash-paren-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slash-paren-in-braced-atpt ()
  "Employ actions of KILL at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slash-paren-in-braced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slash-paren-in-braced-atpt ()
  "Employ actions of LENGTH at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-length (interactive-p)))

(defun ar-parentize-slash-paren-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-parentize (interactive-p)))

(defun ar-quote-slash-paren-in-braced-atpt ()
  "Employ actions of QUOTE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-quote (interactive-p)))

(defun ar-separate-slash-paren-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-separate (interactive-p)))

(defun ar-show-slash-paren-in-braced-atpt ()
  "Employ actions of SHOW at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-show (interactive-p)))

(defun ar-singlequote-slash-paren-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slash-paren-in-braced-atpt ()
  "Employ actions of SLASH at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slash-paren-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slash-paren-in-braced-atpt ()
  "Employ actions of SORT at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-sort (interactive-p)))

(defun ar-trim-slash-paren-in-braced-atpt ()
  "Employ actions of TRIM at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slash-paren-in-braced-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slash-paren-in-braced-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slash-paren-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slash-paren-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of SLASH-PAREN residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'braced 'ar-th-whitespace (interactive-p)))

(defun ar-slash-paren-in-bracketed-atpt ()
  "Employ actions of  at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th (interactive-p)))

(defun ar-greaterangle-slash-paren-in-bracketed-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slash-paren-in-bracketed-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slash-paren-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slash-paren-in-bracketed-atpt ()
  "Employ actions of BACKTICK at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-backtick (interactive-p)))

(defun ar-beg-slash-paren-in-bracketed-atpt ()
  "Employ actions of BEG at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-beg (interactive-p)))

(defun ar-blok-slash-paren-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-blok (interactive-p)))

(defun ar-bounds-slash-paren-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-bounds (interactive-p)))

(defun ar-brace-slash-paren-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-brace (interactive-p)))

(defun ar-bracket-slash-paren-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slash-paren-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-commatize (interactive-p)))

(defun ar-comment-slash-paren-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-comment (interactive-p)))

(defun ar-dollar-slash-paren-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slash-paren-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slash-paren-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slash-paren-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slash-paren-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slash-paren-in-bracketed-atpt ()
  "Employ actions of END at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-end (interactive-p)))

(defun ar-escape-slash-paren-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-escape (interactive-p)))

(defun ar-hide-slash-paren-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slash-paren-in-bracketed-atpt ()
  "Employ actions of HIDESHOW at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slash-paren-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slash-paren-in-bracketed-atpt ()
  "Employ actions of KILL at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slash-paren-in-bracketed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slash-paren-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-length (interactive-p)))

(defun ar-parentize-slash-paren-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-parentize (interactive-p)))

(defun ar-quote-slash-paren-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-quote (interactive-p)))

(defun ar-separate-slash-paren-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-separate (interactive-p)))

(defun ar-show-slash-paren-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-show (interactive-p)))

(defun ar-singlequote-slash-paren-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slash-paren-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slash-paren-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slash-paren-in-bracketed-atpt ()
  "Employ actions of SORT at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-sort (interactive-p)))

(defun ar-trim-slash-paren-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slash-paren-in-bracketed-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slash-paren-in-bracketed-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slash-paren-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slash-paren-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of SLASH-PAREN residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'bracketed 'ar-th-whitespace (interactive-p)))

(defun ar-slash-paren-in-lesserangled-atpt ()
  "Employ actions of  at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th (interactive-p)))

(defun ar-greaterangle-slash-paren-in-lesserangled-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slash-paren-in-lesserangled-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slash-paren-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slash-paren-in-lesserangled-atpt ()
  "Employ actions of BACKTICK at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-slash-paren-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-beg (interactive-p)))

(defun ar-blok-slash-paren-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-slash-paren-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-slash-paren-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-slash-paren-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slash-paren-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-slash-paren-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-slash-paren-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slash-paren-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slash-paren-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slash-paren-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slash-paren-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slash-paren-in-lesserangled-atpt ()
  "Employ actions of END at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-end (interactive-p)))

(defun ar-escape-slash-paren-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-escape (interactive-p)))

(defun ar-hide-slash-paren-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slash-paren-in-lesserangled-atpt ()
  "Employ actions of HIDESHOW at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slash-paren-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slash-paren-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slash-paren-in-lesserangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slash-paren-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-length (interactive-p)))

(defun ar-parentize-slash-paren-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-slash-paren-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-quote (interactive-p)))

(defun ar-separate-slash-paren-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-separate (interactive-p)))

(defun ar-show-slash-paren-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-slash-paren-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slash-paren-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slash-paren-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slash-paren-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-sort (interactive-p)))

(defun ar-trim-slash-paren-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slash-paren-in-lesserangled-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slash-paren-in-lesserangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slash-paren-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slash-paren-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of SLASH-PAREN residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'lesserangled 'ar-th-whitespace (interactive-p)))

(defun ar-slash-paren-in-greaterangled-atpt ()
  "Employ actions of  at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th (interactive-p)))

(defun ar-greaterangle-slash-paren-in-greaterangled-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slash-paren-in-greaterangled-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slash-paren-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slash-paren-in-greaterangled-atpt ()
  "Employ actions of BACKTICK at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-slash-paren-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-beg (interactive-p)))

(defun ar-blok-slash-paren-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-slash-paren-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-slash-paren-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-slash-paren-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slash-paren-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-slash-paren-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-slash-paren-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slash-paren-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slash-paren-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slash-paren-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slash-paren-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slash-paren-in-greaterangled-atpt ()
  "Employ actions of END at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-end (interactive-p)))

(defun ar-escape-slash-paren-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-escape (interactive-p)))

(defun ar-hide-slash-paren-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slash-paren-in-greaterangled-atpt ()
  "Employ actions of HIDESHOW at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slash-paren-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slash-paren-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slash-paren-in-greaterangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slash-paren-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-length (interactive-p)))

(defun ar-parentize-slash-paren-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-slash-paren-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-quote (interactive-p)))

(defun ar-separate-slash-paren-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-separate (interactive-p)))

(defun ar-show-slash-paren-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-slash-paren-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slash-paren-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slash-paren-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slash-paren-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-sort (interactive-p)))

(defun ar-trim-slash-paren-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slash-paren-in-greaterangled-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slash-paren-in-greaterangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slash-paren-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slash-paren-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of SLASH-PAREN residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'greaterangled 'ar-th-whitespace (interactive-p)))

(defun ar-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th (interactive-p)))

(defun ar-greaterangle-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-backtick (interactive-p)))

(defun ar-beg-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-beg (interactive-p)))

(defun ar-blok-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-blok (interactive-p)))

(defun ar-bounds-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-bounds (interactive-p)))

(defun ar-brace-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-brace (interactive-p)))

(defun ar-bracket-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-commatize (interactive-p)))

(defun ar-comment-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-comment (interactive-p)))

(defun ar-dollar-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-end (interactive-p)))

(defun ar-escape-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-escape (interactive-p)))

(defun ar-hide-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-length (interactive-p)))

(defun ar-parentize-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-parentize (interactive-p)))

(defun ar-quote-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-quote (interactive-p)))

(defun ar-separate-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-separate (interactive-p)))

(defun ar-show-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-show (interactive-p)))

(defun ar-singlequote-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-sort (interactive-p)))

(defun ar-trim-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slash-paren-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of SLASH-PAREN residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))

(defun ar-slash-paren-in-parentized-atpt ()
  "Employ actions of  at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th (interactive-p)))

(defun ar-greaterangle-slash-paren-in-parentized-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slash-paren-in-parentized-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slash-paren-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slash-paren-in-parentized-atpt ()
  "Employ actions of BACKTICK at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-backtick (interactive-p)))

(defun ar-beg-slash-paren-in-parentized-atpt ()
  "Employ actions of BEG at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-beg (interactive-p)))

(defun ar-blok-slash-paren-in-parentized-atpt ()
  "Employ actions of BLOK at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-blok (interactive-p)))

(defun ar-bounds-slash-paren-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-bounds (interactive-p)))

(defun ar-brace-slash-paren-in-parentized-atpt ()
  "Employ actions of BRACE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-brace (interactive-p)))

(defun ar-bracket-slash-paren-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slash-paren-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-commatize (interactive-p)))

(defun ar-comment-slash-paren-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-comment (interactive-p)))

(defun ar-dollar-slash-paren-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slash-paren-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slash-paren-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slash-paren-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slash-paren-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slash-paren-in-parentized-atpt ()
  "Employ actions of END at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-end (interactive-p)))

(defun ar-escape-slash-paren-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-escape (interactive-p)))

(defun ar-hide-slash-paren-in-parentized-atpt ()
  "Employ actions of HIDE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slash-paren-in-parentized-atpt ()
  "Employ actions of HIDESHOW at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slash-paren-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slash-paren-in-parentized-atpt ()
  "Employ actions of KILL at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slash-paren-in-parentized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slash-paren-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-length (interactive-p)))

(defun ar-parentize-slash-paren-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-parentize (interactive-p)))

(defun ar-quote-slash-paren-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-quote (interactive-p)))

(defun ar-separate-slash-paren-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-separate (interactive-p)))

(defun ar-show-slash-paren-in-parentized-atpt ()
  "Employ actions of SHOW at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-show (interactive-p)))

(defun ar-singlequote-slash-paren-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slash-paren-in-parentized-atpt ()
  "Employ actions of SLASH at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slash-paren-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slash-paren-in-parentized-atpt ()
  "Employ actions of SORT at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-sort (interactive-p)))

(defun ar-trim-slash-paren-in-parentized-atpt ()
  "Employ actions of TRIM at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slash-paren-in-parentized-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slash-paren-in-parentized-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slash-paren-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slash-paren-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of SLASH-PAREN residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'slash-paren 'parentized 'ar-th-whitespace (interactive-p)))

(defun ar-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of  at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th (interactive-p)))

(defun ar-greaterangle-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of GREATERANGLE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of LESSERANGLE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-backslash (interactive-p)))

(defun ar-backtick-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of BACKTICK at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-backtick (interactive-p)))

(defun ar-beg-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of BEG at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-beg (interactive-p)))

(defun ar-blok-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of BLOK at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-blok (interactive-p)))

(defun ar-bounds-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-bounds (interactive-p)))

(defun ar-brace-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of BRACE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-brace (interactive-p)))

(defun ar-bracket-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of BRACKET at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-bracket (interactive-p)))

(defun ar-commatize-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-commatize (interactive-p)))

(defun ar-comment-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of COMMENT at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-comment (interactive-p)))

(defun ar-dollar-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of END at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-end (interactive-p)))

(defun ar-escape-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-escape (interactive-p)))

(defun ar-hide-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of HIDE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-hide (interactive-p)))

(defun ar-hide-show-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of HIDESHOW at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-hyphen (interactive-p)))

(defun ar-kill-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of KILL at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of LENGTH at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-length (interactive-p)))

(defun ar-parentize-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-parentize (interactive-p)))

(defun ar-quote-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of QUOTE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-quote (interactive-p)))

(defun ar-separate-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-separate (interactive-p)))

(defun ar-show-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of SHOW at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-show (interactive-p)))

(defun ar-singlequote-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-singlequote (interactive-p)))

(defun ar-slash-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of SLASH at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-slash (interactive-p)))

(defun ar-slashparen-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-slashparen (interactive-p)))

(defun ar-sort-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of SORT at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-sort (interactive-p)))

(defun ar-trim-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of TRIM at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-trim (interactive-p)))

(defun ar-trim-left-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of TRIMLEFT at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of TRIMRIGHT at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-xsl-stylesheet-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of XSL-STYLESHEET residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'braced 'ar-th-whitespace (interactive-p)))

(defun ar-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of  at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th (interactive-p)))

(defun ar-greaterangle-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of GREATERANGLE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of LESSERANGLE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-backslash (interactive-p)))

(defun ar-backtick-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of BACKTICK at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-backtick (interactive-p)))

(defun ar-beg-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of BEG at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-beg (interactive-p)))

(defun ar-blok-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-blok (interactive-p)))

(defun ar-bounds-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-bounds (interactive-p)))

(defun ar-brace-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-brace (interactive-p)))

(defun ar-bracket-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-bracket (interactive-p)))

(defun ar-commatize-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-commatize (interactive-p)))

(defun ar-comment-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-comment (interactive-p)))

(defun ar-dollar-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of END at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-end (interactive-p)))

(defun ar-escape-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-escape (interactive-p)))

(defun ar-hide-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-hide (interactive-p)))

(defun ar-hide-show-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of HIDESHOW at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-hyphen (interactive-p)))

(defun ar-kill-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of KILL at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-length (interactive-p)))

(defun ar-parentize-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-parentize (interactive-p)))

(defun ar-quote-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-quote (interactive-p)))

(defun ar-separate-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-separate (interactive-p)))

(defun ar-show-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-show (interactive-p)))

(defun ar-singlequote-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-singlequote (interactive-p)))

(defun ar-slash-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-slash (interactive-p)))

(defun ar-slashparen-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-slashparen (interactive-p)))

(defun ar-sort-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of SORT at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-sort (interactive-p)))

(defun ar-trim-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-trim (interactive-p)))

(defun ar-trim-left-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of TRIMLEFT at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of TRIMRIGHT at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-xsl-stylesheet-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of XSL-STYLESHEET residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'bracketed 'ar-th-whitespace (interactive-p)))

(defun ar-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of  at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th (interactive-p)))

(defun ar-greaterangle-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of GREATERANGLE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of LESSERANGLE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of BACKTICK at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-beg (interactive-p)))

(defun ar-blok-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of END at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-end (interactive-p)))

(defun ar-escape-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-escape (interactive-p)))

(defun ar-hide-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of HIDESHOW at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-length (interactive-p)))

(defun ar-parentize-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-quote (interactive-p)))

(defun ar-separate-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-separate (interactive-p)))

(defun ar-show-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-sort (interactive-p)))

(defun ar-trim-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of TRIMLEFT at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-xsl-stylesheet-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of XSL-STYLESHEET residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'lesserangled 'ar-th-whitespace (interactive-p)))

(defun ar-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of  at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th (interactive-p)))

(defun ar-greaterangle-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of GREATERANGLE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of LESSERANGLE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of BACKTICK at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-beg (interactive-p)))

(defun ar-blok-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of END at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-end (interactive-p)))

(defun ar-escape-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-escape (interactive-p)))

(defun ar-hide-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of HIDESHOW at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-length (interactive-p)))

(defun ar-parentize-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-quote (interactive-p)))

(defun ar-separate-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-separate (interactive-p)))

(defun ar-show-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-sort (interactive-p)))

(defun ar-trim-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of TRIMLEFT at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-xsl-stylesheet-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of XSL-STYLESHEET residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'greaterangled 'ar-th-whitespace (interactive-p)))

(defun ar-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th (interactive-p)))

(defun ar-greaterangle-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-backslash (interactive-p)))

(defun ar-backtick-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-backtick (interactive-p)))

(defun ar-beg-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-beg (interactive-p)))

(defun ar-blok-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-blok (interactive-p)))

(defun ar-bounds-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-bounds (interactive-p)))

(defun ar-brace-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-brace (interactive-p)))

(defun ar-bracket-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-bracket (interactive-p)))

(defun ar-commatize-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-commatize (interactive-p)))

(defun ar-comment-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-comment (interactive-p)))

(defun ar-dollar-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-end (interactive-p)))

(defun ar-escape-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-escape (interactive-p)))

(defun ar-hide-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-hide (interactive-p)))

(defun ar-hide-show-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))

(defun ar-kill-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-length (interactive-p)))

(defun ar-parentize-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-parentize (interactive-p)))

(defun ar-quote-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-quote (interactive-p)))

(defun ar-separate-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-separate (interactive-p)))

(defun ar-show-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-show (interactive-p)))

(defun ar-singlequote-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))

(defun ar-slash-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-slash (interactive-p)))

(defun ar-slashparen-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))

(defun ar-sort-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-sort (interactive-p)))

(defun ar-trim-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-trim (interactive-p)))

(defun ar-trim-left-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-xsl-stylesheet-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of XSL-STYLESHEET residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))

(defun ar-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of  at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th (interactive-p)))

(defun ar-greaterangle-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of GREATERANGLE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of LESSERANGLE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-backslash (interactive-p)))

(defun ar-backtick-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of BACKTICK at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-backtick (interactive-p)))

(defun ar-beg-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of BEG at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-beg (interactive-p)))

(defun ar-blok-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of BLOK at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-blok (interactive-p)))

(defun ar-bounds-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-bounds (interactive-p)))

(defun ar-brace-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of BRACE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-brace (interactive-p)))

(defun ar-bracket-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-bracket (interactive-p)))

(defun ar-commatize-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-commatize (interactive-p)))

(defun ar-comment-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-comment (interactive-p)))

(defun ar-dollar-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of END at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-end (interactive-p)))

(defun ar-escape-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-escape (interactive-p)))

(defun ar-hide-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of HIDE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-hide (interactive-p)))

(defun ar-hide-show-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of HIDESHOW at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-hyphen (interactive-p)))

(defun ar-kill-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of KILL at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-length (interactive-p)))

(defun ar-parentize-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-parentize (interactive-p)))

(defun ar-quote-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-quote (interactive-p)))

(defun ar-separate-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-separate (interactive-p)))

(defun ar-show-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of SHOW at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-show (interactive-p)))

(defun ar-singlequote-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-singlequote (interactive-p)))

(defun ar-slash-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of SLASH at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-slash (interactive-p)))

(defun ar-slashparen-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-slashparen (interactive-p)))

(defun ar-sort-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of SORT at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-sort (interactive-p)))

(defun ar-trim-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of TRIM at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-trim (interactive-p)))

(defun ar-trim-left-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of TRIMLEFT at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of TRIMRIGHT at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-xsl-stylesheet-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of XSL-STYLESHEET residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-stylesheet 'parentized 'ar-th-whitespace (interactive-p)))

(defun ar-xsl-template-in-braced-atpt ()
  "Employ actions of  at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th (interactive-p)))

(defun ar-greaterangle-xsl-template-in-braced-atpt ()
  "Employ actions of GREATERANGLE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-xsl-template-in-braced-atpt ()
  "Employ actions of LESSERANGLE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-xsl-template-in-braced-atpt ()
  "Employ actions of BACKSLASH at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-backslash (interactive-p)))

(defun ar-backtick-xsl-template-in-braced-atpt ()
  "Employ actions of BACKTICK at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-backtick (interactive-p)))

(defun ar-beg-xsl-template-in-braced-atpt ()
  "Employ actions of BEG at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-beg (interactive-p)))

(defun ar-blok-xsl-template-in-braced-atpt ()
  "Employ actions of BLOK at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-blok (interactive-p)))

(defun ar-bounds-xsl-template-in-braced-atpt ()
  "Employ actions of BOUNDS at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-bounds (interactive-p)))

(defun ar-brace-xsl-template-in-braced-atpt ()
  "Employ actions of BRACE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-brace (interactive-p)))

(defun ar-bracket-xsl-template-in-braced-atpt ()
  "Employ actions of BRACKET at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-bracket (interactive-p)))

(defun ar-commatize-xsl-template-in-braced-atpt ()
  "Employ actions of COMMATIZE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-commatize (interactive-p)))

(defun ar-comment-xsl-template-in-braced-atpt ()
  "Employ actions of COMMENT at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-comment (interactive-p)))

(defun ar-dollar-xsl-template-in-braced-atpt ()
  "Employ actions of DOLLAR at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-xsl-template-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-xsl-template-in-braced-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-xsl-template-in-braced-atpt ()
  "Employ actions of DOUBLESLASH at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-xsl-template-in-braced-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-xsl-template-in-braced-atpt ()
  "Employ actions of END at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-end (interactive-p)))

(defun ar-escape-xsl-template-in-braced-atpt ()
  "Employ actions of ESCAPE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-escape (interactive-p)))

(defun ar-hide-xsl-template-in-braced-atpt ()
  "Employ actions of HIDE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-hide (interactive-p)))

(defun ar-hide-show-xsl-template-in-braced-atpt ()
  "Employ actions of HIDESHOW at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-xsl-template-in-braced-atpt ()
  "Employ actions of HYPHEN at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-hyphen (interactive-p)))

(defun ar-kill-xsl-template-in-braced-atpt ()
  "Employ actions of KILL at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-xsl-template-in-braced-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-xsl-template-in-braced-atpt ()
  "Employ actions of LENGTH at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-length (interactive-p)))

(defun ar-parentize-xsl-template-in-braced-atpt ()
  "Employ actions of PARENTIZE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-parentize (interactive-p)))

(defun ar-quote-xsl-template-in-braced-atpt ()
  "Employ actions of QUOTE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-quote (interactive-p)))

(defun ar-separate-xsl-template-in-braced-atpt ()
  "Employ actions of SEPARATE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-separate (interactive-p)))

(defun ar-show-xsl-template-in-braced-atpt ()
  "Employ actions of SHOW at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-show (interactive-p)))

(defun ar-singlequote-xsl-template-in-braced-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-singlequote (interactive-p)))

(defun ar-slash-xsl-template-in-braced-atpt ()
  "Employ actions of SLASH at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-slash (interactive-p)))

(defun ar-slashparen-xsl-template-in-braced-atpt ()
  "Employ actions of SLASHPAREN at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-slashparen (interactive-p)))

(defun ar-sort-xsl-template-in-braced-atpt ()
  "Employ actions of SORT at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-sort (interactive-p)))

(defun ar-trim-xsl-template-in-braced-atpt ()
  "Employ actions of TRIM at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-trim (interactive-p)))

(defun ar-trim-left-xsl-template-in-braced-atpt ()
  "Employ actions of TRIMLEFT at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-xsl-template-in-braced-atpt ()
  "Employ actions of TRIMRIGHT at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-xsl-template-in-braced-atpt ()
  "Employ actions of UNDERSCORE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-xsl-template-in-braced-atpt ()
  "Employ actions of WHITESPACE at things class of XSL-TEMPLATE residing withing BRACED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'braced 'ar-th-whitespace (interactive-p)))

(defun ar-xsl-template-in-bracketed-atpt ()
  "Employ actions of  at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th (interactive-p)))

(defun ar-greaterangle-xsl-template-in-bracketed-atpt ()
  "Employ actions of GREATERANGLE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-xsl-template-in-bracketed-atpt ()
  "Employ actions of LESSERANGLE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-xsl-template-in-bracketed-atpt ()
  "Employ actions of BACKSLASH at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-backslash (interactive-p)))

(defun ar-backtick-xsl-template-in-bracketed-atpt ()
  "Employ actions of BACKTICK at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-backtick (interactive-p)))

(defun ar-beg-xsl-template-in-bracketed-atpt ()
  "Employ actions of BEG at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-beg (interactive-p)))

(defun ar-blok-xsl-template-in-bracketed-atpt ()
  "Employ actions of BLOK at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-blok (interactive-p)))

(defun ar-bounds-xsl-template-in-bracketed-atpt ()
  "Employ actions of BOUNDS at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-bounds (interactive-p)))

(defun ar-brace-xsl-template-in-bracketed-atpt ()
  "Employ actions of BRACE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-brace (interactive-p)))

(defun ar-bracket-xsl-template-in-bracketed-atpt ()
  "Employ actions of BRACKET at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-bracket (interactive-p)))

(defun ar-commatize-xsl-template-in-bracketed-atpt ()
  "Employ actions of COMMATIZE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-commatize (interactive-p)))

(defun ar-comment-xsl-template-in-bracketed-atpt ()
  "Employ actions of COMMENT at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-comment (interactive-p)))

(defun ar-dollar-xsl-template-in-bracketed-atpt ()
  "Employ actions of DOLLAR at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-xsl-template-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-xsl-template-in-bracketed-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-xsl-template-in-bracketed-atpt ()
  "Employ actions of DOUBLESLASH at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-xsl-template-in-bracketed-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-xsl-template-in-bracketed-atpt ()
  "Employ actions of END at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-end (interactive-p)))

(defun ar-escape-xsl-template-in-bracketed-atpt ()
  "Employ actions of ESCAPE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-escape (interactive-p)))

(defun ar-hide-xsl-template-in-bracketed-atpt ()
  "Employ actions of HIDE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-hide (interactive-p)))

(defun ar-hide-show-xsl-template-in-bracketed-atpt ()
  "Employ actions of HIDESHOW at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-xsl-template-in-bracketed-atpt ()
  "Employ actions of HYPHEN at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-hyphen (interactive-p)))

(defun ar-kill-xsl-template-in-bracketed-atpt ()
  "Employ actions of KILL at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-xsl-template-in-bracketed-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-xsl-template-in-bracketed-atpt ()
  "Employ actions of LENGTH at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-length (interactive-p)))

(defun ar-parentize-xsl-template-in-bracketed-atpt ()
  "Employ actions of PARENTIZE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-parentize (interactive-p)))

(defun ar-quote-xsl-template-in-bracketed-atpt ()
  "Employ actions of QUOTE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-quote (interactive-p)))

(defun ar-separate-xsl-template-in-bracketed-atpt ()
  "Employ actions of SEPARATE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-separate (interactive-p)))

(defun ar-show-xsl-template-in-bracketed-atpt ()
  "Employ actions of SHOW at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-show (interactive-p)))

(defun ar-singlequote-xsl-template-in-bracketed-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-singlequote (interactive-p)))

(defun ar-slash-xsl-template-in-bracketed-atpt ()
  "Employ actions of SLASH at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-slash (interactive-p)))

(defun ar-slashparen-xsl-template-in-bracketed-atpt ()
  "Employ actions of SLASHPAREN at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-slashparen (interactive-p)))

(defun ar-sort-xsl-template-in-bracketed-atpt ()
  "Employ actions of SORT at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-sort (interactive-p)))

(defun ar-trim-xsl-template-in-bracketed-atpt ()
  "Employ actions of TRIM at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-trim (interactive-p)))

(defun ar-trim-left-xsl-template-in-bracketed-atpt ()
  "Employ actions of TRIMLEFT at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-xsl-template-in-bracketed-atpt ()
  "Employ actions of TRIMRIGHT at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-xsl-template-in-bracketed-atpt ()
  "Employ actions of UNDERSCORE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-xsl-template-in-bracketed-atpt ()
  "Employ actions of WHITESPACE at things class of XSL-TEMPLATE residing withing BRACKETED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'bracketed 'ar-th-whitespace (interactive-p)))

(defun ar-xsl-template-in-lesserangled-atpt ()
  "Employ actions of  at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th (interactive-p)))

(defun ar-greaterangle-xsl-template-in-lesserangled-atpt ()
  "Employ actions of GREATERANGLE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-xsl-template-in-lesserangled-atpt ()
  "Employ actions of LESSERANGLE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-xsl-template-in-lesserangled-atpt ()
  "Employ actions of BACKSLASH at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-xsl-template-in-lesserangled-atpt ()
  "Employ actions of BACKTICK at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-xsl-template-in-lesserangled-atpt ()
  "Employ actions of BEG at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-beg (interactive-p)))

(defun ar-blok-xsl-template-in-lesserangled-atpt ()
  "Employ actions of BLOK at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-xsl-template-in-lesserangled-atpt ()
  "Employ actions of BOUNDS at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-xsl-template-in-lesserangled-atpt ()
  "Employ actions of BRACE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-xsl-template-in-lesserangled-atpt ()
  "Employ actions of BRACKET at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-xsl-template-in-lesserangled-atpt ()
  "Employ actions of COMMATIZE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-xsl-template-in-lesserangled-atpt ()
  "Employ actions of COMMENT at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-xsl-template-in-lesserangled-atpt ()
  "Employ actions of DOLLAR at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-xsl-template-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-xsl-template-in-lesserangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-xsl-template-in-lesserangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-xsl-template-in-lesserangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-xsl-template-in-lesserangled-atpt ()
  "Employ actions of END at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-end (interactive-p)))

(defun ar-escape-xsl-template-in-lesserangled-atpt ()
  "Employ actions of ESCAPE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-escape (interactive-p)))

(defun ar-hide-xsl-template-in-lesserangled-atpt ()
  "Employ actions of HIDE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-xsl-template-in-lesserangled-atpt ()
  "Employ actions of HIDESHOW at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-xsl-template-in-lesserangled-atpt ()
  "Employ actions of HYPHEN at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-xsl-template-in-lesserangled-atpt ()
  "Employ actions of KILL at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-xsl-template-in-lesserangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-xsl-template-in-lesserangled-atpt ()
  "Employ actions of LENGTH at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-length (interactive-p)))

(defun ar-parentize-xsl-template-in-lesserangled-atpt ()
  "Employ actions of PARENTIZE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-xsl-template-in-lesserangled-atpt ()
  "Employ actions of QUOTE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-quote (interactive-p)))

(defun ar-separate-xsl-template-in-lesserangled-atpt ()
  "Employ actions of SEPARATE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-separate (interactive-p)))

(defun ar-show-xsl-template-in-lesserangled-atpt ()
  "Employ actions of SHOW at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-xsl-template-in-lesserangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-xsl-template-in-lesserangled-atpt ()
  "Employ actions of SLASH at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-xsl-template-in-lesserangled-atpt ()
  "Employ actions of SLASHPAREN at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-xsl-template-in-lesserangled-atpt ()
  "Employ actions of SORT at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-sort (interactive-p)))

(defun ar-trim-xsl-template-in-lesserangled-atpt ()
  "Employ actions of TRIM at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-xsl-template-in-lesserangled-atpt ()
  "Employ actions of TRIMLEFT at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-xsl-template-in-lesserangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-xsl-template-in-lesserangled-atpt ()
  "Employ actions of UNDERSCORE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-xsl-template-in-lesserangled-atpt ()
  "Employ actions of WHITESPACE at things class of XSL-TEMPLATE residing withing LESSER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'lesserangled 'ar-th-whitespace (interactive-p)))

(defun ar-xsl-template-in-greaterangled-atpt ()
  "Employ actions of  at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th (interactive-p)))

(defun ar-greaterangle-xsl-template-in-greaterangled-atpt ()
  "Employ actions of GREATERANGLE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-xsl-template-in-greaterangled-atpt ()
  "Employ actions of LESSERANGLE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-xsl-template-in-greaterangled-atpt ()
  "Employ actions of BACKSLASH at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-backslash (interactive-p)))

(defun ar-backtick-xsl-template-in-greaterangled-atpt ()
  "Employ actions of BACKTICK at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-backtick (interactive-p)))

(defun ar-beg-xsl-template-in-greaterangled-atpt ()
  "Employ actions of BEG at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-beg (interactive-p)))

(defun ar-blok-xsl-template-in-greaterangled-atpt ()
  "Employ actions of BLOK at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-blok (interactive-p)))

(defun ar-bounds-xsl-template-in-greaterangled-atpt ()
  "Employ actions of BOUNDS at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-bounds (interactive-p)))

(defun ar-brace-xsl-template-in-greaterangled-atpt ()
  "Employ actions of BRACE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-brace (interactive-p)))

(defun ar-bracket-xsl-template-in-greaterangled-atpt ()
  "Employ actions of BRACKET at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-bracket (interactive-p)))

(defun ar-commatize-xsl-template-in-greaterangled-atpt ()
  "Employ actions of COMMATIZE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-commatize (interactive-p)))

(defun ar-comment-xsl-template-in-greaterangled-atpt ()
  "Employ actions of COMMENT at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-comment (interactive-p)))

(defun ar-dollar-xsl-template-in-greaterangled-atpt ()
  "Employ actions of DOLLAR at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-xsl-template-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-xsl-template-in-greaterangled-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-xsl-template-in-greaterangled-atpt ()
  "Employ actions of DOUBLESLASH at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-xsl-template-in-greaterangled-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-xsl-template-in-greaterangled-atpt ()
  "Employ actions of END at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-end (interactive-p)))

(defun ar-escape-xsl-template-in-greaterangled-atpt ()
  "Employ actions of ESCAPE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-escape (interactive-p)))

(defun ar-hide-xsl-template-in-greaterangled-atpt ()
  "Employ actions of HIDE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-hide (interactive-p)))

(defun ar-hide-show-xsl-template-in-greaterangled-atpt ()
  "Employ actions of HIDESHOW at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-xsl-template-in-greaterangled-atpt ()
  "Employ actions of HYPHEN at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-hyphen (interactive-p)))

(defun ar-kill-xsl-template-in-greaterangled-atpt ()
  "Employ actions of KILL at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-xsl-template-in-greaterangled-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-xsl-template-in-greaterangled-atpt ()
  "Employ actions of LENGTH at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-length (interactive-p)))

(defun ar-parentize-xsl-template-in-greaterangled-atpt ()
  "Employ actions of PARENTIZE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-parentize (interactive-p)))

(defun ar-quote-xsl-template-in-greaterangled-atpt ()
  "Employ actions of QUOTE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-quote (interactive-p)))

(defun ar-separate-xsl-template-in-greaterangled-atpt ()
  "Employ actions of SEPARATE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-separate (interactive-p)))

(defun ar-show-xsl-template-in-greaterangled-atpt ()
  "Employ actions of SHOW at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-show (interactive-p)))

(defun ar-singlequote-xsl-template-in-greaterangled-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-singlequote (interactive-p)))

(defun ar-slash-xsl-template-in-greaterangled-atpt ()
  "Employ actions of SLASH at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-slash (interactive-p)))

(defun ar-slashparen-xsl-template-in-greaterangled-atpt ()
  "Employ actions of SLASHPAREN at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-slashparen (interactive-p)))

(defun ar-sort-xsl-template-in-greaterangled-atpt ()
  "Employ actions of SORT at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-sort (interactive-p)))

(defun ar-trim-xsl-template-in-greaterangled-atpt ()
  "Employ actions of TRIM at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-trim (interactive-p)))

(defun ar-trim-left-xsl-template-in-greaterangled-atpt ()
  "Employ actions of TRIMLEFT at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-xsl-template-in-greaterangled-atpt ()
  "Employ actions of TRIMRIGHT at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-xsl-template-in-greaterangled-atpt ()
  "Employ actions of UNDERSCORE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-xsl-template-in-greaterangled-atpt ()
  "Employ actions of WHITESPACE at things class of XSL-TEMPLATE residing withing GREATER-ANGLED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'greaterangled 'ar-th-whitespace (interactive-p)))

(defun ar-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of  at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th (interactive-p)))

(defun ar-greaterangle-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of GREATERANGLE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of LESSERANGLE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKSLASH at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-backslash (interactive-p)))

(defun ar-backtick-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of BACKTICK at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-backtick (interactive-p)))

(defun ar-beg-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of BEG at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-beg (interactive-p)))

(defun ar-blok-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of BLOK at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-blok (interactive-p)))

(defun ar-bounds-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of BOUNDS at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-bounds (interactive-p)))

(defun ar-brace-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-brace (interactive-p)))

(defun ar-bracket-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of BRACKET at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-bracket (interactive-p)))

(defun ar-commatize-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMATIZE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-commatize (interactive-p)))

(defun ar-comment-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of COMMENT at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-comment (interactive-p)))

(defun ar-dollar-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of DOLLAR at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLESLASH at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of END at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-end (interactive-p)))

(defun ar-escape-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of ESCAPE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-escape (interactive-p)))

(defun ar-hide-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-hide (interactive-p)))

(defun ar-hide-show-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of HIDESHOW at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of HYPHEN at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-hyphen (interactive-p)))

(defun ar-kill-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of KILL at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of LENGTH at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-length (interactive-p)))

(defun ar-parentize-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of PARENTIZE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-parentize (interactive-p)))

(defun ar-quote-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of QUOTE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-quote (interactive-p)))

(defun ar-separate-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of SEPARATE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-separate (interactive-p)))

(defun ar-show-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of SHOW at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-show (interactive-p)))

(defun ar-singlequote-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-singlequote (interactive-p)))

(defun ar-slash-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASH at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-slash (interactive-p)))

(defun ar-slashparen-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of SLASHPAREN at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-slashparen (interactive-p)))

(defun ar-sort-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of SORT at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-sort (interactive-p)))

(defun ar-trim-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIM at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-trim (interactive-p)))

(defun ar-trim-left-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMLEFT at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of TRIMRIGHT at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of UNDERSCORE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-xsl-template-in-left-right-singlequoted-atpt ()
  "Employ actions of WHITESPACE at things class of XSL-TEMPLATE residing withing LEFT-RIGHT-SINGLEQUOTED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'left-right-singlequoted 'ar-th-whitespace (interactive-p)))

(defun ar-xsl-template-in-parentized-atpt ()
  "Employ actions of  at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th (interactive-p)))

(defun ar-greaterangle-xsl-template-in-parentized-atpt ()
  "Employ actions of GREATERANGLE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-xsl-template-in-parentized-atpt ()
  "Employ actions of LESSERANGLE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-xsl-template-in-parentized-atpt ()
  "Employ actions of BACKSLASH at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-backslash (interactive-p)))

(defun ar-backtick-xsl-template-in-parentized-atpt ()
  "Employ actions of BACKTICK at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-backtick (interactive-p)))

(defun ar-beg-xsl-template-in-parentized-atpt ()
  "Employ actions of BEG at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-beg (interactive-p)))

(defun ar-blok-xsl-template-in-parentized-atpt ()
  "Employ actions of BLOK at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-blok (interactive-p)))

(defun ar-bounds-xsl-template-in-parentized-atpt ()
  "Employ actions of BOUNDS at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-bounds (interactive-p)))

(defun ar-brace-xsl-template-in-parentized-atpt ()
  "Employ actions of BRACE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-brace (interactive-p)))

(defun ar-bracket-xsl-template-in-parentized-atpt ()
  "Employ actions of BRACKET at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-bracket (interactive-p)))

(defun ar-commatize-xsl-template-in-parentized-atpt ()
  "Employ actions of COMMATIZE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-commatize (interactive-p)))

(defun ar-comment-xsl-template-in-parentized-atpt ()
  "Employ actions of COMMENT at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-comment (interactive-p)))

(defun ar-dollar-xsl-template-in-parentized-atpt ()
  "Employ actions of DOLLAR at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-xsl-template-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-xsl-template-in-parentized-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-xsl-template-in-parentized-atpt ()
  "Employ actions of DOUBLESLASH at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-xsl-template-in-parentized-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-xsl-template-in-parentized-atpt ()
  "Employ actions of END at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-end (interactive-p)))

(defun ar-escape-xsl-template-in-parentized-atpt ()
  "Employ actions of ESCAPE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-escape (interactive-p)))

(defun ar-hide-xsl-template-in-parentized-atpt ()
  "Employ actions of HIDE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-hide (interactive-p)))

(defun ar-hide-show-xsl-template-in-parentized-atpt ()
  "Employ actions of HIDESHOW at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-xsl-template-in-parentized-atpt ()
  "Employ actions of HYPHEN at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-hyphen (interactive-p)))

(defun ar-kill-xsl-template-in-parentized-atpt ()
  "Employ actions of KILL at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-xsl-template-in-parentized-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-xsl-template-in-parentized-atpt ()
  "Employ actions of LENGTH at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-length (interactive-p)))

(defun ar-parentize-xsl-template-in-parentized-atpt ()
  "Employ actions of PARENTIZE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-parentize (interactive-p)))

(defun ar-quote-xsl-template-in-parentized-atpt ()
  "Employ actions of QUOTE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-quote (interactive-p)))

(defun ar-separate-xsl-template-in-parentized-atpt ()
  "Employ actions of SEPARATE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-separate (interactive-p)))

(defun ar-show-xsl-template-in-parentized-atpt ()
  "Employ actions of SHOW at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-show (interactive-p)))

(defun ar-singlequote-xsl-template-in-parentized-atpt ()
  "Employ actions of SINGLEQUOTE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-singlequote (interactive-p)))

(defun ar-slash-xsl-template-in-parentized-atpt ()
  "Employ actions of SLASH at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-slash (interactive-p)))

(defun ar-slashparen-xsl-template-in-parentized-atpt ()
  "Employ actions of SLASHPAREN at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-slashparen (interactive-p)))

(defun ar-sort-xsl-template-in-parentized-atpt ()
  "Employ actions of SORT at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-sort (interactive-p)))

(defun ar-trim-xsl-template-in-parentized-atpt ()
  "Employ actions of TRIM at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-trim (interactive-p)))

(defun ar-trim-left-xsl-template-in-parentized-atpt ()
  "Employ actions of TRIMLEFT at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-xsl-template-in-parentized-atpt ()
  "Employ actions of TRIMRIGHT at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-xsl-template-in-parentized-atpt ()
  "Employ actions of UNDERSCORE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-xsl-template-in-parentized-atpt ()
  "Employ actions of WHITESPACE at things class of XSL-TEMPLATE residing withing PARENTIZED. "
  (interactive "*")
  (ar-thing-in-thing 'xsl-template 'parentized 'ar-th-whitespace (interactive-p)))

(provide 'ar-thingatpt-data-forms-aktiv-in-delimited-list)
;;;thing-data-forms-aktiv-in-delimited-list.el ends here

