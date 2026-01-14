;;; thing-unpaired-delimited-list-in-data-forms-aktiv.el --- thing-in-thing functions -*- lexical-binding: t; -*-
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

(defun ar-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backslashed-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-backslashed-in-blok-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-backslashed-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backslashed-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backslashed-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backslashed-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-backslashed-in-blok-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-backslashed-in-blok-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-backslashed-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-backslashed-in-blok-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-backslashed-in-blok-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backslashed-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-backslashed-in-blok-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-backslashed-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backslashed-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backslashed-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backslashed-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backslashed-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backslashed-in-blok-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-backslashed-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-backslashed-in-blok-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backslashed-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backslashed-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backslashed-in-blok-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backslashed-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backslashed-in-blok-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-backslashed-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-backslashed-in-blok-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-backslashed-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-backslashed-in-blok-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-backslashed-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backslashed-in-blok-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backslashed-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backslashed-in-blok-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-backslashed-in-blok-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backslashed-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backslashed-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backslashed-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backslashed-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-backslashed-in-double-backslash-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-backslashed-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backslashed-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backslashed-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backslashed-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-backslashed-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-backslashed-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-backslashed-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-backslashed-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-backslashed-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backslashed-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-backslashed-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-backslashed-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backslashed-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backslashed-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backslashed-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backslashed-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backslashed-in-double-backslash-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-backslashed-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-backslashed-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backslashed-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backslashed-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backslashed-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backslashed-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backslashed-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-backslashed-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-backslashed-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-backslashed-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-backslashed-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-backslashed-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backslashed-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backslashed-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backslashed-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-backslashed-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backslashed-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backslashed-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backslashed-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backslashed-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-backslashed-in-doubleslash-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-backslashed-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backslashed-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backslashed-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backslashed-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-backslashed-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-backslashed-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-backslashed-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-backslashed-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-backslashed-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backslashed-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-backslashed-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-backslashed-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backslashed-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backslashed-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backslashed-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backslashed-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backslashed-in-doubleslash-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-backslashed-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-backslashed-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backslashed-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backslashed-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backslashed-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backslashed-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backslashed-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-backslashed-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-backslashed-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-backslashed-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-backslashed-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-backslashed-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backslashed-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backslashed-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backslashed-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-backslashed-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backslashed-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backslashed-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backslashed-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backslashed-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backslashed-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-backslashed-in-tabledata-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-backslashed-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backslashed-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backslashed-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backslashed-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-backslashed-in-tabledata-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-backslashed-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-backslashed-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-backslashed-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-backslashed-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backslashed-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-backslashed-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-backslashed-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backslashed-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backslashed-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backslashed-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backslashed-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backslashed-in-tabledata-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-backslashed-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-backslashed-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backslashed-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backslashed-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backslashed-in-tabledata-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backslashed-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backslashed-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-backslashed-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-backslashed-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-backslashed-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-backslashed-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-backslashed-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backslashed-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backslashed-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backslashed-in-tabledata-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-backslashed-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backslashed-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backslashed-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backslashed-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backslashed-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-backslashed-in-slash-paren-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-backslashed-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backslashed-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backslashed-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backslashed-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-backslashed-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-backslashed-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-backslashed-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-backslashed-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-backslashed-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backslashed-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-backslashed-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-backslashed-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backslashed-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backslashed-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backslashed-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backslashed-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backslashed-in-slash-paren-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-backslashed-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-backslashed-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backslashed-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backslashed-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backslashed-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backslashed-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backslashed-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-backslashed-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-backslashed-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-backslashed-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-backslashed-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-backslashed-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backslashed-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backslashed-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backslashed-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-backslashed-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backslashed-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backslashed-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backslashed-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backslashed-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backslashed-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-backslashed-in-xsl-template-atpt ()
  "Employ actions of  at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-backslashed-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backslashed-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backslashed-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backslashed-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-backslashed-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-backslashed-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-backslashed-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-backslashed-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-backslashed-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backslashed-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-backslashed-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-backslashed-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backslashed-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backslashed-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backslashed-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backslashed-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backslashed-in-xsl-template-atpt ()
  "Employ actions of END at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-backslashed-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-backslashed-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backslashed-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backslashed-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backslashed-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backslashed-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backslashed-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-backslashed-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-backslashed-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-backslashed-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-backslashed-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-backslashed-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backslashed-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backslashed-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backslashed-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-backslashed-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backslashed-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backslashed-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backslashed-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backslashed-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of BACKSLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backslashed 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-backticked-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-backticked-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backticked-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backticked-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backticked-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-backticked-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-backticked-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-backticked-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-backticked-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-backticked-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backticked-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-backticked-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-backticked-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backticked-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backticked-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backticked-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backticked-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backticked-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-backticked-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-backticked-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backticked-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backticked-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backticked-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backticked-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backticked-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-backticked-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-backticked-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-backticked-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-backticked-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-backticked-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backticked-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backticked-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backticked-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-backticked-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backticked-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backticked-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backticked-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backticked-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-backticked-in-blok-atpt ()
  "Employ actions of  at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-backticked-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backticked-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backticked-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backticked-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-backticked-in-blok-atpt ()
  "Employ actions of BEG at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-backticked-in-blok-atpt ()
  "Employ actions of BLOK at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-backticked-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-backticked-in-blok-atpt ()
  "Employ actions of BRACE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-backticked-in-blok-atpt ()
  "Employ actions of BRACKET at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backticked-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-backticked-in-blok-atpt ()
  "Employ actions of COMMENT at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-backticked-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backticked-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backticked-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backticked-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backticked-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backticked-in-blok-atpt ()
  "Employ actions of END at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-backticked-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-backticked-in-blok-atpt ()
  "Employ actions of HIDE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backticked-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backticked-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backticked-in-blok-atpt ()
  "Employ actions of KILL at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backticked-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backticked-in-blok-atpt ()
  "Employ actions of LENGTH at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-backticked-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-backticked-in-blok-atpt ()
  "Employ actions of QUOTE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-backticked-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-backticked-in-blok-atpt ()
  "Employ actions of SHOW at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-backticked-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backticked-in-blok-atpt ()
  "Employ actions of SLASH at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backticked-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backticked-in-blok-atpt ()
  "Employ actions of SORT at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-backticked-in-blok-atpt ()
  "Employ actions of TRIM at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backticked-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backticked-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backticked-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backticked-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-backticked-in-double-backslash-atpt ()
  "Employ actions of  at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-backticked-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backticked-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backticked-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backticked-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-backticked-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-backticked-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-backticked-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-backticked-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-backticked-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backticked-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-backticked-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-backticked-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backticked-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backticked-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backticked-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backticked-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backticked-in-double-backslash-atpt ()
  "Employ actions of END at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-backticked-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-backticked-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backticked-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backticked-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backticked-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backticked-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backticked-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-backticked-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-backticked-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-backticked-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-backticked-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-backticked-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backticked-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backticked-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backticked-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-backticked-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backticked-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backticked-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backticked-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backticked-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-backticked-in-doubleslash-atpt ()
  "Employ actions of  at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-backticked-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backticked-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backticked-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backticked-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-backticked-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-backticked-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-backticked-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-backticked-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-backticked-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backticked-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-backticked-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-backticked-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backticked-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backticked-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backticked-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backticked-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backticked-in-doubleslash-atpt ()
  "Employ actions of END at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-backticked-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-backticked-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backticked-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backticked-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backticked-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backticked-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backticked-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-backticked-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-backticked-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-backticked-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-backticked-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-backticked-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backticked-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backticked-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backticked-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-backticked-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backticked-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backticked-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backticked-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backticked-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backticked-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-backticked-in-tabledata-atpt ()
  "Employ actions of  at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-backticked-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backticked-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backticked-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backticked-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-backticked-in-tabledata-atpt ()
  "Employ actions of BEG at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-backticked-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-backticked-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-backticked-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-backticked-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backticked-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-backticked-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-backticked-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backticked-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backticked-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backticked-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backticked-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backticked-in-tabledata-atpt ()
  "Employ actions of END at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-backticked-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-backticked-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backticked-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backticked-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backticked-in-tabledata-atpt ()
  "Employ actions of KILL at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backticked-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backticked-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-backticked-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-backticked-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-backticked-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-backticked-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-backticked-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backticked-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backticked-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backticked-in-tabledata-atpt ()
  "Employ actions of SORT at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-backticked-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backticked-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backticked-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backticked-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backticked-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-backticked-in-slash-paren-atpt ()
  "Employ actions of  at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-backticked-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backticked-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backticked-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backticked-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-backticked-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-backticked-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-backticked-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-backticked-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-backticked-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backticked-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-backticked-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-backticked-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backticked-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backticked-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backticked-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backticked-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backticked-in-slash-paren-atpt ()
  "Employ actions of END at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-backticked-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-backticked-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backticked-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backticked-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backticked-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backticked-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backticked-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-backticked-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-backticked-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-backticked-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-backticked-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-backticked-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backticked-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backticked-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backticked-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-backticked-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backticked-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backticked-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backticked-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backticked-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backticked-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-backticked-in-xsl-template-atpt ()
  "Employ actions of  at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-backticked-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-backticked-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-backticked-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-backticked-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-backticked-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-backticked-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-backticked-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-backticked-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-backticked-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-backticked-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-backticked-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-backticked-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-backticked-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-backticked-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-backticked-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-backticked-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-backticked-in-xsl-template-atpt ()
  "Employ actions of END at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-backticked-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-backticked-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-backticked-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-backticked-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-backticked-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-backticked-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-backticked-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-backticked-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-backticked-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-backticked-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-backticked-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-backticked-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-backticked-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-backticked-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-backticked-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-backticked-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-backticked-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-backticked-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-backticked-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-backticked-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of BACKTICKED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'backticked 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-dollared-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-dollared-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-dollared-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-dollared-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-dollared-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-dollared-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-dollared-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-dollared-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-dollared-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-dollared-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-dollared-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-dollared-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-dollared-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-dollared-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-dollared-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-dollared-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-dollared-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-dollared-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-dollared-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-dollared-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-dollared-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-dollared-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-dollared-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-dollared-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-dollared-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-dollared-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-dollared-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-dollared-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-dollared-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-dollared-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-dollared-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-dollared-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-dollared-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-dollared-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-dollared-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-dollared-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-dollared-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-dollared-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-dollared-in-blok-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-dollared-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-dollared-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-dollared-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-dollared-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-dollared-in-blok-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-dollared-in-blok-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-dollared-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-dollared-in-blok-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-dollared-in-blok-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-dollared-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-dollared-in-blok-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-dollared-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-dollared-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-dollared-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-dollared-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-dollared-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-dollared-in-blok-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-dollared-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-dollared-in-blok-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-dollared-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-dollared-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-dollared-in-blok-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-dollared-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-dollared-in-blok-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-dollared-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-dollared-in-blok-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-dollared-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-dollared-in-blok-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-dollared-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-dollared-in-blok-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-dollared-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-dollared-in-blok-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-dollared-in-blok-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-dollared-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-dollared-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-dollared-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-dollared-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-dollared-in-double-backslash-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-dollared-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-dollared-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-dollared-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-dollared-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-dollared-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-dollared-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-dollared-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-dollared-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-dollared-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-dollared-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-dollared-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-dollared-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-dollared-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-dollared-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-dollared-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-dollared-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-dollared-in-double-backslash-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-dollared-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-dollared-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-dollared-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-dollared-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-dollared-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-dollared-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-dollared-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-dollared-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-dollared-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-dollared-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-dollared-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-dollared-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-dollared-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-dollared-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-dollared-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-dollared-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-dollared-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-dollared-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-dollared-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-dollared-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-dollared-in-doubleslash-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-dollared-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-dollared-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-dollared-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-dollared-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-dollared-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-dollared-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-dollared-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-dollared-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-dollared-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-dollared-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-dollared-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-dollared-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-dollared-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-dollared-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-dollared-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-dollared-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-dollared-in-doubleslash-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-dollared-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-dollared-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-dollared-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-dollared-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-dollared-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-dollared-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-dollared-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-dollared-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-dollared-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-dollared-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-dollared-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-dollared-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-dollared-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-dollared-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-dollared-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-dollared-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-dollared-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-dollared-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-dollared-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-dollared-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-dollared-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-dollared-in-tabledata-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-dollared-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-dollared-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-dollared-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-dollared-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-dollared-in-tabledata-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-dollared-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-dollared-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-dollared-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-dollared-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-dollared-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-dollared-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-dollared-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-dollared-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-dollared-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-dollared-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-dollared-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-dollared-in-tabledata-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-dollared-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-dollared-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-dollared-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-dollared-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-dollared-in-tabledata-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-dollared-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-dollared-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-dollared-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-dollared-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-dollared-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-dollared-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-dollared-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-dollared-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-dollared-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-dollared-in-tabledata-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-dollared-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-dollared-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-dollared-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-dollared-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-dollared-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-dollared-in-slash-paren-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-dollared-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-dollared-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-dollared-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-dollared-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-dollared-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-dollared-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-dollared-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-dollared-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-dollared-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-dollared-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-dollared-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-dollared-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-dollared-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-dollared-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-dollared-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-dollared-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-dollared-in-slash-paren-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-dollared-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-dollared-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-dollared-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-dollared-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-dollared-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-dollared-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-dollared-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-dollared-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-dollared-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-dollared-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-dollared-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-dollared-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-dollared-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-dollared-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-dollared-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-dollared-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-dollared-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-dollared-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-dollared-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-dollared-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-dollared-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-dollared-in-xsl-template-atpt ()
  "Employ actions of  at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-dollared-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-dollared-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-dollared-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-dollared-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-dollared-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-dollared-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-dollared-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-dollared-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-dollared-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-dollared-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-dollared-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-dollared-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-dollared-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-dollared-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-dollared-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-dollared-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-dollared-in-xsl-template-atpt ()
  "Employ actions of END at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-dollared-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-dollared-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-dollared-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-dollared-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-dollared-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-dollared-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-dollared-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-dollared-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-dollared-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-dollared-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-dollared-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-dollared-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-dollared-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-dollared-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-dollared-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-dollared-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-dollared-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-dollared-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-dollared-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-dollared-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of DOLLARED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'dollared 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doublequoted-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-doublequoted-in-blok-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-doublequoted-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doublequoted-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doublequoted-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doublequoted-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-doublequoted-in-blok-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-doublequoted-in-blok-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-doublequoted-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-doublequoted-in-blok-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-doublequoted-in-blok-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doublequoted-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-doublequoted-in-blok-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-doublequoted-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doublequoted-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doublequoted-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doublequoted-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doublequoted-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doublequoted-in-blok-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-doublequoted-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-doublequoted-in-blok-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doublequoted-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doublequoted-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doublequoted-in-blok-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doublequoted-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doublequoted-in-blok-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-doublequoted-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-doublequoted-in-blok-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-doublequoted-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-doublequoted-in-blok-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-doublequoted-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doublequoted-in-blok-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doublequoted-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doublequoted-in-blok-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-doublequoted-in-blok-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doublequoted-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doublequoted-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doublequoted-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doublequoted-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-doublequoted-in-double-backslash-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-doublequoted-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doublequoted-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doublequoted-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doublequoted-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-doublequoted-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-doublequoted-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-doublequoted-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-doublequoted-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-doublequoted-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doublequoted-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-doublequoted-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-doublequoted-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doublequoted-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doublequoted-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doublequoted-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doublequoted-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doublequoted-in-double-backslash-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-doublequoted-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-doublequoted-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doublequoted-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doublequoted-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doublequoted-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doublequoted-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doublequoted-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-doublequoted-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-doublequoted-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-doublequoted-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-doublequoted-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-doublequoted-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doublequoted-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doublequoted-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doublequoted-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-doublequoted-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doublequoted-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doublequoted-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doublequoted-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doublequoted-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-doublequoted-in-doubleslash-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-doublequoted-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doublequoted-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doublequoted-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doublequoted-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-doublequoted-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-doublequoted-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-doublequoted-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-doublequoted-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-doublequoted-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doublequoted-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-doublequoted-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-doublequoted-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doublequoted-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doublequoted-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doublequoted-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doublequoted-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doublequoted-in-doubleslash-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-doublequoted-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-doublequoted-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doublequoted-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doublequoted-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doublequoted-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doublequoted-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doublequoted-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-doublequoted-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-doublequoted-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-doublequoted-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-doublequoted-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-doublequoted-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doublequoted-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doublequoted-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doublequoted-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-doublequoted-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doublequoted-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doublequoted-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doublequoted-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doublequoted-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doublequoted-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-doublequoted-in-tabledata-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-doublequoted-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doublequoted-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doublequoted-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doublequoted-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-doublequoted-in-tabledata-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-doublequoted-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-doublequoted-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-doublequoted-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-doublequoted-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doublequoted-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-doublequoted-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-doublequoted-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doublequoted-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doublequoted-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doublequoted-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doublequoted-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doublequoted-in-tabledata-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-doublequoted-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-doublequoted-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doublequoted-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doublequoted-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doublequoted-in-tabledata-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doublequoted-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doublequoted-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-doublequoted-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-doublequoted-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-doublequoted-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-doublequoted-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-doublequoted-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doublequoted-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doublequoted-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doublequoted-in-tabledata-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-doublequoted-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doublequoted-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doublequoted-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doublequoted-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doublequoted-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-doublequoted-in-slash-paren-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-doublequoted-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doublequoted-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doublequoted-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doublequoted-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-doublequoted-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-doublequoted-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-doublequoted-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-doublequoted-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-doublequoted-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doublequoted-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-doublequoted-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-doublequoted-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doublequoted-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doublequoted-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doublequoted-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doublequoted-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doublequoted-in-slash-paren-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-doublequoted-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-doublequoted-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doublequoted-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doublequoted-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doublequoted-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doublequoted-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doublequoted-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-doublequoted-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-doublequoted-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-doublequoted-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-doublequoted-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-doublequoted-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doublequoted-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doublequoted-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doublequoted-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-doublequoted-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doublequoted-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doublequoted-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doublequoted-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doublequoted-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doublequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-doublequoted-in-xsl-template-atpt ()
  "Employ actions of  at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-doublequoted-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-doublequoted-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-doublequoted-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-doublequoted-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-doublequoted-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-doublequoted-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-doublequoted-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-doublequoted-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-doublequoted-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-doublequoted-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-doublequoted-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-doublequoted-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-doublequoted-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-doublequoted-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-doublequoted-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-doublequoted-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-doublequoted-in-xsl-template-atpt ()
  "Employ actions of END at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-doublequoted-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-doublequoted-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-doublequoted-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-doublequoted-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-doublequoted-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-doublequoted-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-doublequoted-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-doublequoted-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-doublequoted-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-doublequoted-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-doublequoted-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-doublequoted-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-doublequoted-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-doublequoted-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-doublequoted-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-doublequoted-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-doublequoted-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-doublequoted-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-doublequoted-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-doublequoted-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of DOUBLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'doublequoted 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-equalized-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-equalized-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-equalized-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-equalized-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-equalized-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-equalized-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-equalized-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-equalized-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-equalized-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-equalized-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-equalized-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-equalized-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-equalized-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-equalized-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-equalized-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-equalized-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-equalized-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-equalized-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-equalized-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-equalized-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-equalized-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-equalized-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-equalized-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-equalized-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-equalized-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-equalized-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-equalized-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-equalized-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-equalized-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-equalized-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-equalized-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-equalized-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-equalized-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-equalized-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-equalized-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-equalized-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-equalized-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-equalized-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-equalized-in-blok-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-equalized-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-equalized-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-equalized-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-equalized-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-equalized-in-blok-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-equalized-in-blok-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-equalized-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-equalized-in-blok-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-equalized-in-blok-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-equalized-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-equalized-in-blok-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-equalized-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-equalized-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-equalized-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-equalized-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-equalized-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-equalized-in-blok-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-equalized-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-equalized-in-blok-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-equalized-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-equalized-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-equalized-in-blok-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-equalized-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-equalized-in-blok-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-equalized-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-equalized-in-blok-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-equalized-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-equalized-in-blok-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-equalized-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-equalized-in-blok-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-equalized-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-equalized-in-blok-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-equalized-in-blok-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-equalized-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-equalized-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-equalized-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-equalized-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-equalized-in-double-backslash-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-equalized-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-equalized-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-equalized-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-equalized-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-equalized-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-equalized-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-equalized-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-equalized-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-equalized-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-equalized-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-equalized-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-equalized-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-equalized-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-equalized-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-equalized-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-equalized-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-equalized-in-double-backslash-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-equalized-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-equalized-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-equalized-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-equalized-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-equalized-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-equalized-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-equalized-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-equalized-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-equalized-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-equalized-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-equalized-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-equalized-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-equalized-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-equalized-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-equalized-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-equalized-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-equalized-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-equalized-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-equalized-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-equalized-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-equalized-in-doubleslash-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-equalized-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-equalized-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-equalized-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-equalized-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-equalized-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-equalized-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-equalized-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-equalized-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-equalized-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-equalized-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-equalized-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-equalized-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-equalized-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-equalized-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-equalized-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-equalized-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-equalized-in-doubleslash-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-equalized-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-equalized-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-equalized-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-equalized-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-equalized-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-equalized-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-equalized-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-equalized-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-equalized-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-equalized-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-equalized-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-equalized-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-equalized-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-equalized-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-equalized-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-equalized-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-equalized-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-equalized-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-equalized-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-equalized-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-equalized-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-equalized-in-tabledata-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-equalized-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-equalized-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-equalized-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-equalized-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-equalized-in-tabledata-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-equalized-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-equalized-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-equalized-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-equalized-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-equalized-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-equalized-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-equalized-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-equalized-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-equalized-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-equalized-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-equalized-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-equalized-in-tabledata-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-equalized-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-equalized-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-equalized-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-equalized-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-equalized-in-tabledata-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-equalized-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-equalized-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-equalized-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-equalized-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-equalized-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-equalized-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-equalized-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-equalized-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-equalized-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-equalized-in-tabledata-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-equalized-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-equalized-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-equalized-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-equalized-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-equalized-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-equalized-in-slash-paren-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-equalized-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-equalized-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-equalized-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-equalized-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-equalized-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-equalized-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-equalized-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-equalized-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-equalized-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-equalized-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-equalized-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-equalized-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-equalized-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-equalized-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-equalized-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-equalized-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-equalized-in-slash-paren-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-equalized-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-equalized-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-equalized-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-equalized-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-equalized-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-equalized-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-equalized-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-equalized-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-equalized-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-equalized-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-equalized-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-equalized-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-equalized-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-equalized-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-equalized-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-equalized-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-equalized-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-equalized-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-equalized-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-equalized-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-equalized-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-equalized-in-xsl-template-atpt ()
  "Employ actions of  at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-equalized-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-equalized-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-equalized-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-equalized-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-equalized-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-equalized-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-equalized-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-equalized-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-equalized-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-equalized-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-equalized-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-equalized-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-equalized-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-equalized-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-equalized-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-equalized-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-equalized-in-xsl-template-atpt ()
  "Employ actions of END at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-equalized-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-equalized-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-equalized-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-equalized-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-equalized-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-equalized-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-equalized-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-equalized-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-equalized-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-equalized-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-equalized-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-equalized-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-equalized-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-equalized-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-equalized-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-equalized-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-equalized-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-equalized-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-equalized-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-equalized-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of EQUALIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'equalized 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-hyphened-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-hyphened-in-blok-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-hyphened-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-hyphened-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-hyphened-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-hyphened-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-hyphened-in-blok-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-hyphened-in-blok-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-hyphened-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-hyphened-in-blok-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-hyphened-in-blok-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-hyphened-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-hyphened-in-blok-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-hyphened-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-hyphened-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-hyphened-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-hyphened-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-hyphened-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-hyphened-in-blok-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-hyphened-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-hyphened-in-blok-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-hyphened-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-hyphened-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-hyphened-in-blok-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-hyphened-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-hyphened-in-blok-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-hyphened-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-hyphened-in-blok-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-hyphened-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-hyphened-in-blok-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-hyphened-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-hyphened-in-blok-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-hyphened-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-hyphened-in-blok-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-hyphened-in-blok-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-hyphened-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-hyphened-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-hyphened-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-hyphened-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-hyphened-in-double-backslash-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-hyphened-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-hyphened-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-hyphened-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-hyphened-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-hyphened-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-hyphened-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-hyphened-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-hyphened-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-hyphened-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-hyphened-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-hyphened-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-hyphened-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-hyphened-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-hyphened-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-hyphened-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-hyphened-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-hyphened-in-double-backslash-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-hyphened-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-hyphened-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-hyphened-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-hyphened-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-hyphened-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-hyphened-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-hyphened-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-hyphened-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-hyphened-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-hyphened-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-hyphened-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-hyphened-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-hyphened-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-hyphened-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-hyphened-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-hyphened-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-hyphened-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-hyphened-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-hyphened-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-hyphened-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-hyphened-in-doubleslash-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-hyphened-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-hyphened-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-hyphened-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-hyphened-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-hyphened-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-hyphened-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-hyphened-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-hyphened-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-hyphened-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-hyphened-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-hyphened-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-hyphened-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-hyphened-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-hyphened-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-hyphened-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-hyphened-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-hyphened-in-doubleslash-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-hyphened-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-hyphened-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-hyphened-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-hyphened-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-hyphened-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-hyphened-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-hyphened-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-hyphened-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-hyphened-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-hyphened-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-hyphened-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-hyphened-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-hyphened-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-hyphened-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-hyphened-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-hyphened-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-hyphened-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-hyphened-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-hyphened-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-hyphened-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-hyphened-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-hyphened-in-tabledata-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-hyphened-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-hyphened-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-hyphened-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-hyphened-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-hyphened-in-tabledata-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-hyphened-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-hyphened-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-hyphened-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-hyphened-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-hyphened-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-hyphened-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-hyphened-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-hyphened-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-hyphened-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-hyphened-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-hyphened-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-hyphened-in-tabledata-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-hyphened-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-hyphened-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-hyphened-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-hyphened-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-hyphened-in-tabledata-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-hyphened-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-hyphened-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-hyphened-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-hyphened-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-hyphened-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-hyphened-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-hyphened-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-hyphened-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-hyphened-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-hyphened-in-tabledata-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-hyphened-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-hyphened-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-hyphened-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-hyphened-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-hyphened-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-hyphened-in-slash-paren-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-hyphened-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-hyphened-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-hyphened-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-hyphened-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-hyphened-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-hyphened-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-hyphened-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-hyphened-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-hyphened-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-hyphened-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-hyphened-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-hyphened-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-hyphened-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-hyphened-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-hyphened-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-hyphened-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-hyphened-in-slash-paren-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-hyphened-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-hyphened-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-hyphened-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-hyphened-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-hyphened-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-hyphened-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-hyphened-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-hyphened-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-hyphened-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-hyphened-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-hyphened-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-hyphened-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-hyphened-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-hyphened-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-hyphened-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-hyphened-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-hyphened-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-hyphened-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-hyphened-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-hyphened-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-hyphened-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-hyphened-in-xsl-template-atpt ()
  "Employ actions of  at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-hyphened-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-hyphened-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-hyphened-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-hyphened-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-hyphened-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-hyphened-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-hyphened-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-hyphened-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-hyphened-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-hyphened-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-hyphened-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-hyphened-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-hyphened-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-hyphened-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-hyphened-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-hyphened-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-hyphened-in-xsl-template-atpt ()
  "Employ actions of END at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-hyphened-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-hyphened-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-hyphened-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-hyphened-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-hyphened-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-hyphened-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-hyphened-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-hyphened-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-hyphened-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-hyphened-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-hyphened-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-hyphened-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-hyphened-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-hyphened-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-hyphened-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-hyphened-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-hyphened-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-hyphened-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-hyphened-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-hyphened-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of HYPHENED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'hyphened 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-singlequoted-in-blok-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-singlequoted-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-singlequoted-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-singlequoted-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-singlequoted-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-singlequoted-in-blok-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-singlequoted-in-blok-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-singlequoted-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-singlequoted-in-blok-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-singlequoted-in-blok-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-singlequoted-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-singlequoted-in-blok-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-singlequoted-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-singlequoted-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-singlequoted-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-singlequoted-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-singlequoted-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-singlequoted-in-blok-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-singlequoted-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-singlequoted-in-blok-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-singlequoted-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-singlequoted-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-singlequoted-in-blok-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-singlequoted-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-singlequoted-in-blok-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-singlequoted-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-singlequoted-in-blok-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-singlequoted-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-singlequoted-in-blok-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-singlequoted-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-singlequoted-in-blok-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-singlequoted-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-singlequoted-in-blok-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-singlequoted-in-blok-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-singlequoted-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-singlequoted-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-singlequoted-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-singlequoted-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-singlequoted-in-double-backslash-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-singlequoted-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-singlequoted-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-singlequoted-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-singlequoted-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-singlequoted-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-singlequoted-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-singlequoted-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-singlequoted-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-singlequoted-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-singlequoted-in-double-backslash-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-singlequoted-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-singlequoted-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-singlequoted-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-singlequoted-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-singlequoted-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-singlequoted-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-singlequoted-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-singlequoted-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-singlequoted-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-singlequoted-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-singlequoted-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-singlequoted-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-singlequoted-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-singlequoted-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-singlequoted-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-singlequoted-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-singlequoted-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-singlequoted-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-singlequoted-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-singlequoted-in-doubleslash-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-singlequoted-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-singlequoted-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-singlequoted-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-singlequoted-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-singlequoted-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-singlequoted-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-singlequoted-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-singlequoted-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-singlequoted-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-singlequoted-in-doubleslash-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-singlequoted-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-singlequoted-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-singlequoted-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-singlequoted-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-singlequoted-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-singlequoted-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-singlequoted-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-singlequoted-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-singlequoted-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-singlequoted-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-singlequoted-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-singlequoted-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-singlequoted-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-singlequoted-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-singlequoted-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-singlequoted-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-singlequoted-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-singlequoted-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-singlequoted-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-singlequoted-in-tabledata-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-singlequoted-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-singlequoted-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-singlequoted-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-singlequoted-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-singlequoted-in-tabledata-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-singlequoted-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-singlequoted-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-singlequoted-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-singlequoted-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-singlequoted-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-singlequoted-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-singlequoted-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-singlequoted-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-singlequoted-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-singlequoted-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-singlequoted-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-singlequoted-in-tabledata-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-singlequoted-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-singlequoted-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-singlequoted-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-singlequoted-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-singlequoted-in-tabledata-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-singlequoted-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-singlequoted-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-singlequoted-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-singlequoted-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-singlequoted-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-singlequoted-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-singlequoted-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-singlequoted-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-singlequoted-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-singlequoted-in-tabledata-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-singlequoted-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-singlequoted-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-singlequoted-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-singlequoted-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-singlequoted-in-slash-paren-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-singlequoted-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-singlequoted-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-singlequoted-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-singlequoted-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-singlequoted-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-singlequoted-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-singlequoted-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-singlequoted-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-singlequoted-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-singlequoted-in-slash-paren-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-singlequoted-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-singlequoted-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-singlequoted-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-singlequoted-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-singlequoted-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-singlequoted-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-singlequoted-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-singlequoted-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-singlequoted-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-singlequoted-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-singlequoted-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-singlequoted-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-singlequoted-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-singlequoted-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-singlequoted-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-singlequoted-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-singlequoted-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-singlequoted-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-singlequoted-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-singlequoted-in-xsl-template-atpt ()
  "Employ actions of  at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-singlequoted-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-singlequoted-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-singlequoted-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-singlequoted-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-singlequoted-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-singlequoted-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-singlequoted-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-singlequoted-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-singlequoted-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-singlequoted-in-xsl-template-atpt ()
  "Employ actions of END at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-singlequoted-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-singlequoted-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-singlequoted-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-singlequoted-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-singlequoted-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-singlequoted-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-singlequoted-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-singlequoted-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-singlequoted-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-singlequoted-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-singlequoted-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-singlequoted-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-singlequoted-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-singlequoted-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-singlequoted-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-singlequoted-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-singlequoted-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-singlequoted-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-singlequoted-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'singlequoted 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-slashed-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-slashed-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slashed-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slashed-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slashed-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-slashed-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-slashed-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-slashed-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-slashed-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-slashed-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slashed-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-slashed-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-slashed-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slashed-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slashed-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slashed-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slashed-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slashed-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-slashed-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-slashed-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slashed-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slashed-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slashed-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slashed-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slashed-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-slashed-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-slashed-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-slashed-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-slashed-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-slashed-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slashed-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slashed-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slashed-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-slashed-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slashed-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slashed-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slashed-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slashed-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-slashed-in-blok-atpt ()
  "Employ actions of  at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-slashed-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slashed-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slashed-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slashed-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-slashed-in-blok-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-slashed-in-blok-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-slashed-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-slashed-in-blok-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-slashed-in-blok-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slashed-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-slashed-in-blok-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-slashed-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slashed-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slashed-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slashed-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slashed-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slashed-in-blok-atpt ()
  "Employ actions of END at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-slashed-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-slashed-in-blok-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slashed-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slashed-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slashed-in-blok-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slashed-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slashed-in-blok-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-slashed-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-slashed-in-blok-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-slashed-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-slashed-in-blok-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-slashed-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slashed-in-blok-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slashed-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slashed-in-blok-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-slashed-in-blok-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slashed-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slashed-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slashed-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slashed-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-slashed-in-double-backslash-atpt ()
  "Employ actions of  at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-slashed-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slashed-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slashed-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slashed-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-slashed-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-slashed-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-slashed-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-slashed-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-slashed-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slashed-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-slashed-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-slashed-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slashed-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slashed-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slashed-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slashed-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slashed-in-double-backslash-atpt ()
  "Employ actions of END at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-slashed-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-slashed-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slashed-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slashed-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slashed-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slashed-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slashed-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-slashed-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-slashed-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-slashed-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-slashed-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-slashed-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slashed-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slashed-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slashed-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-slashed-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slashed-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slashed-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slashed-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slashed-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-slashed-in-doubleslash-atpt ()
  "Employ actions of  at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-slashed-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slashed-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slashed-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slashed-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-slashed-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-slashed-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-slashed-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-slashed-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-slashed-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slashed-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-slashed-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-slashed-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slashed-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slashed-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slashed-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slashed-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slashed-in-doubleslash-atpt ()
  "Employ actions of END at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-slashed-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-slashed-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slashed-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slashed-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slashed-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slashed-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slashed-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-slashed-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-slashed-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-slashed-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-slashed-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-slashed-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slashed-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slashed-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slashed-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-slashed-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slashed-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slashed-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slashed-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slashed-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slashed-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-slashed-in-tabledata-atpt ()
  "Employ actions of  at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-slashed-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slashed-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slashed-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slashed-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-slashed-in-tabledata-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-slashed-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-slashed-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-slashed-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-slashed-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slashed-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-slashed-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-slashed-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slashed-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slashed-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slashed-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slashed-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slashed-in-tabledata-atpt ()
  "Employ actions of END at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-slashed-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-slashed-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slashed-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slashed-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slashed-in-tabledata-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slashed-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slashed-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-slashed-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-slashed-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-slashed-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-slashed-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-slashed-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slashed-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slashed-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slashed-in-tabledata-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-slashed-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slashed-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slashed-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slashed-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slashed-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-slashed-in-slash-paren-atpt ()
  "Employ actions of  at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-slashed-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slashed-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slashed-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slashed-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-slashed-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-slashed-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-slashed-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-slashed-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-slashed-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slashed-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-slashed-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-slashed-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slashed-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slashed-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slashed-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slashed-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slashed-in-slash-paren-atpt ()
  "Employ actions of END at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-slashed-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-slashed-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slashed-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slashed-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slashed-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slashed-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slashed-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-slashed-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-slashed-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-slashed-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-slashed-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-slashed-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slashed-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slashed-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slashed-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-slashed-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slashed-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slashed-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slashed-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slashed-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slashed-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-slashed-in-xsl-template-atpt ()
  "Employ actions of  at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-slashed-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-slashed-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-slashed-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-slashed-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-slashed-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-slashed-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-slashed-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-slashed-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-slashed-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-slashed-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-slashed-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-slashed-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-slashed-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-slashed-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-slashed-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-slashed-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-slashed-in-xsl-template-atpt ()
  "Employ actions of END at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-slashed-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-slashed-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-slashed-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-slashed-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-slashed-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-slashed-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-slashed-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-slashed-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-slashed-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-slashed-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-slashed-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-slashed-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-slashed-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-slashed-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-slashed-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-slashed-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-slashed-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-slashed-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-slashed-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-slashed-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of SLASHED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'slashed 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-underscored-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-underscored-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-underscored-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-underscored-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-underscored-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-underscored-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-underscored-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-underscored-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-underscored-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-underscored-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-underscored-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-underscored-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-underscored-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-underscored-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-underscored-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-underscored-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-underscored-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-underscored-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-underscored-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-underscored-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-underscored-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-underscored-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-underscored-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-underscored-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-underscored-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-underscored-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-underscored-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-underscored-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-underscored-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-underscored-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-underscored-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-underscored-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-underscored-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-underscored-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-underscored-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-underscored-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-underscored-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-underscored-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-underscored-in-blok-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-underscored-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-underscored-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-underscored-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-underscored-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-underscored-in-blok-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-underscored-in-blok-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-underscored-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-underscored-in-blok-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-underscored-in-blok-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-underscored-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-underscored-in-blok-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-underscored-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-underscored-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-underscored-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-underscored-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-underscored-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-underscored-in-blok-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-underscored-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-underscored-in-blok-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-underscored-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-underscored-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-underscored-in-blok-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-underscored-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-underscored-in-blok-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-underscored-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-underscored-in-blok-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-underscored-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-underscored-in-blok-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-underscored-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-underscored-in-blok-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-underscored-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-underscored-in-blok-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-underscored-in-blok-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-underscored-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-underscored-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-underscored-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-underscored-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-underscored-in-double-backslash-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-underscored-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-underscored-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-underscored-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-underscored-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-underscored-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-underscored-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-underscored-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-underscored-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-underscored-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-underscored-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-underscored-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-underscored-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-underscored-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-underscored-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-underscored-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-underscored-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-underscored-in-double-backslash-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-underscored-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-underscored-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-underscored-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-underscored-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-underscored-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-underscored-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-underscored-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-underscored-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-underscored-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-underscored-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-underscored-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-underscored-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-underscored-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-underscored-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-underscored-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-underscored-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-underscored-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-underscored-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-underscored-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-underscored-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-underscored-in-doubleslash-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-underscored-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-underscored-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-underscored-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-underscored-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-underscored-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-underscored-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-underscored-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-underscored-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-underscored-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-underscored-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-underscored-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-underscored-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-underscored-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-underscored-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-underscored-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-underscored-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-underscored-in-doubleslash-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-underscored-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-underscored-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-underscored-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-underscored-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-underscored-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-underscored-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-underscored-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-underscored-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-underscored-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-underscored-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-underscored-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-underscored-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-underscored-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-underscored-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-underscored-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-underscored-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-underscored-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-underscored-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-underscored-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-underscored-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-underscored-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-underscored-in-tabledata-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-underscored-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-underscored-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-underscored-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-underscored-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-underscored-in-tabledata-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-underscored-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-underscored-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-underscored-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-underscored-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-underscored-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-underscored-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-underscored-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-underscored-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-underscored-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-underscored-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-underscored-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-underscored-in-tabledata-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-underscored-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-underscored-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-underscored-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-underscored-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-underscored-in-tabledata-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-underscored-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-underscored-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-underscored-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-underscored-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-underscored-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-underscored-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-underscored-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-underscored-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-underscored-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-underscored-in-tabledata-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-underscored-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-underscored-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-underscored-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-underscored-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-underscored-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-underscored-in-slash-paren-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-underscored-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-underscored-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-underscored-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-underscored-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-underscored-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-underscored-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-underscored-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-underscored-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-underscored-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-underscored-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-underscored-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-underscored-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-underscored-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-underscored-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-underscored-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-underscored-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-underscored-in-slash-paren-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-underscored-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-underscored-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-underscored-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-underscored-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-underscored-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-underscored-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-underscored-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-underscored-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-underscored-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-underscored-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-underscored-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-underscored-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-underscored-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-underscored-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-underscored-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-underscored-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-underscored-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-underscored-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-underscored-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-underscored-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-underscored-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-underscored-in-xsl-template-atpt ()
  "Employ actions of  at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-underscored-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-underscored-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-underscored-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-underscored-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-underscored-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-underscored-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-underscored-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-underscored-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-underscored-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-underscored-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-underscored-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-underscored-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-underscored-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-underscored-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-underscored-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-underscored-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-underscored-in-xsl-template-atpt ()
  "Employ actions of END at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-underscored-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-underscored-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-underscored-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-underscored-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-underscored-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-underscored-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-underscored-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-underscored-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-underscored-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-underscored-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-underscored-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-underscored-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-underscored-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-underscored-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-underscored-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-underscored-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-underscored-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-underscored-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-underscored-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-underscored-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of UNDERSCORED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'underscored 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-whitespaced-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-whitespaced-in-blok-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-whitespaced-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-whitespaced-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-whitespaced-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-whitespaced-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-whitespaced-in-blok-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-whitespaced-in-blok-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-whitespaced-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-whitespaced-in-blok-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-whitespaced-in-blok-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-whitespaced-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-whitespaced-in-blok-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-whitespaced-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-whitespaced-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-whitespaced-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-whitespaced-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-whitespaced-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-whitespaced-in-blok-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-whitespaced-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-whitespaced-in-blok-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-whitespaced-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-whitespaced-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-whitespaced-in-blok-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-whitespaced-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-whitespaced-in-blok-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-whitespaced-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-whitespaced-in-blok-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-whitespaced-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-whitespaced-in-blok-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-whitespaced-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-whitespaced-in-blok-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-whitespaced-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-whitespaced-in-blok-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-whitespaced-in-blok-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-whitespaced-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-whitespaced-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-whitespaced-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-whitespaced-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-whitespaced-in-double-backslash-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-whitespaced-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-whitespaced-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-whitespaced-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-whitespaced-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-whitespaced-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-whitespaced-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-whitespaced-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-whitespaced-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-whitespaced-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-whitespaced-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-whitespaced-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-whitespaced-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-whitespaced-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-whitespaced-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-whitespaced-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-whitespaced-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-whitespaced-in-double-backslash-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-whitespaced-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-whitespaced-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-whitespaced-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-whitespaced-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-whitespaced-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-whitespaced-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-whitespaced-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-whitespaced-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-whitespaced-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-whitespaced-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-whitespaced-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-whitespaced-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-whitespaced-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-whitespaced-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-whitespaced-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-whitespaced-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-whitespaced-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-whitespaced-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-whitespaced-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-whitespaced-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-whitespaced-in-doubleslash-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-whitespaced-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-whitespaced-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-whitespaced-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-whitespaced-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-whitespaced-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-whitespaced-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-whitespaced-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-whitespaced-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-whitespaced-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-whitespaced-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-whitespaced-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-whitespaced-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-whitespaced-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-whitespaced-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-whitespaced-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-whitespaced-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-whitespaced-in-doubleslash-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-whitespaced-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-whitespaced-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-whitespaced-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-whitespaced-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-whitespaced-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-whitespaced-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-whitespaced-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-whitespaced-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-whitespaced-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-whitespaced-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-whitespaced-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-whitespaced-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-whitespaced-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-whitespaced-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-whitespaced-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-whitespaced-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-whitespaced-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-whitespaced-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-whitespaced-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-whitespaced-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-whitespaced-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-whitespaced-in-tabledata-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-whitespaced-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-whitespaced-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-whitespaced-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-whitespaced-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-whitespaced-in-tabledata-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-whitespaced-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-whitespaced-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-whitespaced-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-whitespaced-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-whitespaced-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-whitespaced-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-whitespaced-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-whitespaced-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-whitespaced-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-whitespaced-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-whitespaced-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-whitespaced-in-tabledata-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-whitespaced-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-whitespaced-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-whitespaced-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-whitespaced-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-whitespaced-in-tabledata-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-whitespaced-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-whitespaced-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-whitespaced-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-whitespaced-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-whitespaced-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-whitespaced-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-whitespaced-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-whitespaced-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-whitespaced-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-whitespaced-in-tabledata-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-whitespaced-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-whitespaced-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-whitespaced-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-whitespaced-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-whitespaced-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-whitespaced-in-slash-paren-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-whitespaced-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-whitespaced-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-whitespaced-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-whitespaced-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-whitespaced-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-whitespaced-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-whitespaced-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-whitespaced-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-whitespaced-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-whitespaced-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-whitespaced-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-whitespaced-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-whitespaced-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-whitespaced-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-whitespaced-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-whitespaced-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-whitespaced-in-slash-paren-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-whitespaced-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-whitespaced-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-whitespaced-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-whitespaced-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-whitespaced-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-whitespaced-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-whitespaced-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-whitespaced-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-whitespaced-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-whitespaced-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-whitespaced-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-whitespaced-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-whitespaced-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-whitespaced-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-whitespaced-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-whitespaced-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-whitespaced-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-whitespaced-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-whitespaced-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-whitespaced-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-whitespaced-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-whitespaced-in-xsl-template-atpt ()
  "Employ actions of  at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-whitespaced-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-whitespaced-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-whitespaced-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-whitespaced-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-whitespaced-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-whitespaced-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-whitespaced-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-whitespaced-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-whitespaced-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-whitespaced-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-whitespaced-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-whitespaced-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-whitespaced-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-whitespaced-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-whitespaced-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-whitespaced-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-whitespaced-in-xsl-template-atpt ()
  "Employ actions of END at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-whitespaced-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-whitespaced-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-whitespaced-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-whitespaced-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-whitespaced-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-whitespaced-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-whitespaced-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-whitespaced-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-whitespaced-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-whitespaced-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-whitespaced-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-whitespaced-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-whitespaced-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-whitespaced-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-whitespaced-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-whitespaced-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-whitespaced-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-whitespaced-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-whitespaced-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-whitespaced-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of WHITESPACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'whitespaced 'xsl-template 'ar-th-whitespace (interactive-p)))

(provide 'ar-thingatpt-unpaired-delimited-list-in-data-forms-aktiv)
;;;thing-unpaired-delimited-list-in-data-forms-aktiv.el ends here

