;;; thing-delimited-list-in-data-forms-aktiv.el --- thing-in-thing functions -*- lexical-binding: t; -*-
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

(defun ar-braced-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-braced-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-braced-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-braced-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-braced-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-braced-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-braced-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-braced-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-braced-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-braced-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-braced-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-braced-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-braced-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-braced-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-braced-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-braced-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-braced-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-braced-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-braced-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-braced-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-braced-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-braced-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-braced-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-braced-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-braced-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-braced-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-braced-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-braced-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-braced-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-braced-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-braced-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-braced-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-braced-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-braced-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-braced-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-braced-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-braced-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-braced-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-braced-in-blok-atpt ()
  "Employ actions of  at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-braced-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-braced-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-braced-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-braced-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-braced-in-blok-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-braced-in-blok-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-braced-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-braced-in-blok-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-braced-in-blok-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-braced-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-braced-in-blok-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-braced-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-braced-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-braced-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-braced-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-braced-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-braced-in-blok-atpt ()
  "Employ actions of END at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-braced-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-braced-in-blok-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-braced-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-braced-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-braced-in-blok-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-braced-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-braced-in-blok-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-braced-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-braced-in-blok-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-braced-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-braced-in-blok-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-braced-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-braced-in-blok-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-braced-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-braced-in-blok-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-braced-in-blok-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-braced-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-braced-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-braced-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-braced-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-braced-in-double-backslash-atpt ()
  "Employ actions of  at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-braced-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-braced-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-braced-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-braced-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-braced-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-braced-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-braced-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-braced-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-braced-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-braced-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-braced-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-braced-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-braced-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-braced-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-braced-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-braced-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-braced-in-double-backslash-atpt ()
  "Employ actions of END at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-braced-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-braced-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-braced-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-braced-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-braced-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-braced-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-braced-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-braced-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-braced-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-braced-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-braced-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-braced-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-braced-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-braced-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-braced-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-braced-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-braced-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-braced-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-braced-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-braced-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-braced-in-doubleslash-atpt ()
  "Employ actions of  at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-braced-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-braced-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-braced-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-braced-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-braced-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-braced-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-braced-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-braced-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-braced-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-braced-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-braced-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-braced-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-braced-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-braced-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-braced-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-braced-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-braced-in-doubleslash-atpt ()
  "Employ actions of END at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-braced-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-braced-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-braced-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-braced-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-braced-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-braced-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-braced-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-braced-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-braced-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-braced-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-braced-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-braced-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-braced-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-braced-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-braced-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-braced-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-braced-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-braced-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-braced-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-braced-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-braced-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-braced-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-braced-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-braced-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-braced-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-braced-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-braced-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-braced-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-braced-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-braced-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-braced-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-braced-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-braced-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-braced-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-braced-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-braced-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-braced-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-braced-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-braced-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-braced-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-braced-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-braced-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-braced-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-braced-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-braced-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-braced-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-braced-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-braced-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-braced-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-braced-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-braced-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-braced-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-braced-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-braced-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-braced-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-braced-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-braced-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-braced-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-braced-in-tabledata-atpt ()
  "Employ actions of  at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-braced-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-braced-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-braced-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-braced-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-braced-in-tabledata-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-braced-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-braced-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-braced-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-braced-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-braced-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-braced-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-braced-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-braced-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-braced-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-braced-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-braced-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-braced-in-tabledata-atpt ()
  "Employ actions of END at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-braced-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-braced-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-braced-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-braced-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-braced-in-tabledata-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-braced-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-braced-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-braced-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-braced-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-braced-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-braced-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-braced-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-braced-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-braced-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-braced-in-tabledata-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-braced-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-braced-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-braced-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-braced-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-braced-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-braced-in-slash-paren-atpt ()
  "Employ actions of  at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-braced-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-braced-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-braced-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-braced-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-braced-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-braced-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-braced-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-braced-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-braced-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-braced-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-braced-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-braced-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-braced-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-braced-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-braced-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-braced-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-braced-in-slash-paren-atpt ()
  "Employ actions of END at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-braced-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-braced-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-braced-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-braced-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-braced-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-braced-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-braced-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-braced-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-braced-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-braced-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-braced-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-braced-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-braced-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-braced-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-braced-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-braced-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-braced-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-braced-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-braced-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-braced-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-braced-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-braced-in-xsl-template-atpt ()
  "Employ actions of  at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-braced-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-braced-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-braced-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-braced-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-braced-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-braced-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-braced-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-braced-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-braced-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-braced-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-braced-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-braced-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-braced-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-braced-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-braced-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-braced-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-braced-in-xsl-template-atpt ()
  "Employ actions of END at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-braced-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-braced-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-braced-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-braced-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-braced-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-braced-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-braced-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-braced-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-braced-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-braced-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-braced-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-braced-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-braced-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-braced-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-braced-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-braced-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-braced-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-braced-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-braced-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-braced-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of BRACED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'braced 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-bracketed-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-bracketed-in-blok-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-bracketed-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-bracketed-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-bracketed-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-bracketed-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-bracketed-in-blok-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-bracketed-in-blok-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-bracketed-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-bracketed-in-blok-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-bracketed-in-blok-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-bracketed-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-bracketed-in-blok-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-bracketed-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-bracketed-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-bracketed-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-bracketed-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-bracketed-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-bracketed-in-blok-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-bracketed-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-bracketed-in-blok-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-bracketed-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-bracketed-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-bracketed-in-blok-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-bracketed-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-bracketed-in-blok-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-bracketed-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-bracketed-in-blok-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-bracketed-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-bracketed-in-blok-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-bracketed-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-bracketed-in-blok-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-bracketed-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-bracketed-in-blok-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-bracketed-in-blok-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-bracketed-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-bracketed-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-bracketed-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-bracketed-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-bracketed-in-double-backslash-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-bracketed-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-bracketed-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-bracketed-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-bracketed-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-bracketed-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-bracketed-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-bracketed-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-bracketed-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-bracketed-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-bracketed-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-bracketed-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-bracketed-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-bracketed-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-bracketed-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-bracketed-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-bracketed-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-bracketed-in-double-backslash-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-bracketed-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-bracketed-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-bracketed-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-bracketed-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-bracketed-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-bracketed-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-bracketed-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-bracketed-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-bracketed-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-bracketed-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-bracketed-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-bracketed-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-bracketed-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-bracketed-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-bracketed-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-bracketed-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-bracketed-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-bracketed-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-bracketed-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-bracketed-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-bracketed-in-doubleslash-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-bracketed-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-bracketed-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-bracketed-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-bracketed-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-bracketed-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-bracketed-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-bracketed-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-bracketed-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-bracketed-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-bracketed-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-bracketed-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-bracketed-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-bracketed-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-bracketed-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-bracketed-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-bracketed-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-bracketed-in-doubleslash-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-bracketed-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-bracketed-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-bracketed-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-bracketed-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-bracketed-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-bracketed-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-bracketed-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-bracketed-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-bracketed-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-bracketed-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-bracketed-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-bracketed-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-bracketed-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-bracketed-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-bracketed-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-bracketed-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-bracketed-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-bracketed-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-bracketed-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-bracketed-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-bracketed-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-bracketed-in-tabledata-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-bracketed-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-bracketed-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-bracketed-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-bracketed-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-bracketed-in-tabledata-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-bracketed-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-bracketed-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-bracketed-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-bracketed-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-bracketed-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-bracketed-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-bracketed-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-bracketed-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-bracketed-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-bracketed-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-bracketed-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-bracketed-in-tabledata-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-bracketed-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-bracketed-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-bracketed-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-bracketed-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-bracketed-in-tabledata-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-bracketed-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-bracketed-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-bracketed-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-bracketed-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-bracketed-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-bracketed-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-bracketed-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-bracketed-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-bracketed-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-bracketed-in-tabledata-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-bracketed-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-bracketed-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-bracketed-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-bracketed-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-bracketed-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-bracketed-in-slash-paren-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-bracketed-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-bracketed-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-bracketed-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-bracketed-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-bracketed-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-bracketed-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-bracketed-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-bracketed-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-bracketed-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-bracketed-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-bracketed-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-bracketed-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-bracketed-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-bracketed-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-bracketed-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-bracketed-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-bracketed-in-slash-paren-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-bracketed-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-bracketed-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-bracketed-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-bracketed-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-bracketed-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-bracketed-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-bracketed-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-bracketed-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-bracketed-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-bracketed-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-bracketed-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-bracketed-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-bracketed-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-bracketed-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-bracketed-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-bracketed-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-bracketed-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-bracketed-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-bracketed-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-bracketed-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-bracketed-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-bracketed-in-xsl-template-atpt ()
  "Employ actions of  at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-bracketed-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-bracketed-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-bracketed-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-bracketed-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-bracketed-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-bracketed-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-bracketed-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-bracketed-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-bracketed-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-bracketed-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-bracketed-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-bracketed-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-bracketed-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-bracketed-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-bracketed-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-bracketed-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-bracketed-in-xsl-template-atpt ()
  "Employ actions of END at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-bracketed-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-bracketed-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-bracketed-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-bracketed-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-bracketed-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-bracketed-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-bracketed-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-bracketed-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-bracketed-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-bracketed-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-bracketed-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-bracketed-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-bracketed-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-bracketed-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-bracketed-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-bracketed-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-bracketed-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-bracketed-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-bracketed-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-bracketed-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of BRACKETED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'bracketed 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-lesserangled-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-lesserangled-in-blok-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-lesserangled-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-lesserangled-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-lesserangled-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-lesserangled-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-lesserangled-in-blok-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-lesserangled-in-blok-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-lesserangled-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-lesserangled-in-blok-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-lesserangled-in-blok-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-lesserangled-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-lesserangled-in-blok-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-lesserangled-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-lesserangled-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-lesserangled-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-lesserangled-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-lesserangled-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-lesserangled-in-blok-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-lesserangled-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-lesserangled-in-blok-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-lesserangled-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-lesserangled-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-lesserangled-in-blok-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-lesserangled-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-lesserangled-in-blok-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-lesserangled-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-lesserangled-in-blok-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-lesserangled-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-lesserangled-in-blok-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-lesserangled-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-lesserangled-in-blok-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-lesserangled-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-lesserangled-in-blok-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-lesserangled-in-blok-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-lesserangled-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-lesserangled-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-lesserangled-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-lesserangled-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-lesserangled-in-double-backslash-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-lesserangled-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-lesserangled-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-lesserangled-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-lesserangled-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-lesserangled-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-lesserangled-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-lesserangled-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-lesserangled-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-lesserangled-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-lesserangled-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-lesserangled-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-lesserangled-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-lesserangled-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-lesserangled-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-lesserangled-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-lesserangled-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-lesserangled-in-double-backslash-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-lesserangled-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-lesserangled-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-lesserangled-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-lesserangled-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-lesserangled-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-lesserangled-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-lesserangled-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-lesserangled-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-lesserangled-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-lesserangled-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-lesserangled-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-lesserangled-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-lesserangled-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-lesserangled-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-lesserangled-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-lesserangled-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-lesserangled-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-lesserangled-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-lesserangled-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-lesserangled-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-lesserangled-in-doubleslash-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-lesserangled-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-lesserangled-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-lesserangled-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-lesserangled-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-lesserangled-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-lesserangled-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-lesserangled-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-lesserangled-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-lesserangled-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-lesserangled-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-lesserangled-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-lesserangled-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-lesserangled-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-lesserangled-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-lesserangled-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-lesserangled-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-lesserangled-in-doubleslash-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-lesserangled-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-lesserangled-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-lesserangled-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-lesserangled-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-lesserangled-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-lesserangled-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-lesserangled-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-lesserangled-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-lesserangled-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-lesserangled-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-lesserangled-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-lesserangled-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-lesserangled-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-lesserangled-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-lesserangled-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-lesserangled-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-lesserangled-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-lesserangled-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-lesserangled-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-lesserangled-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-lesserangled-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-lesserangled-in-tabledata-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-lesserangled-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-lesserangled-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-lesserangled-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-lesserangled-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-lesserangled-in-tabledata-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-lesserangled-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-lesserangled-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-lesserangled-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-lesserangled-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-lesserangled-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-lesserangled-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-lesserangled-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-lesserangled-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-lesserangled-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-lesserangled-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-lesserangled-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-lesserangled-in-tabledata-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-lesserangled-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-lesserangled-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-lesserangled-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-lesserangled-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-lesserangled-in-tabledata-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-lesserangled-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-lesserangled-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-lesserangled-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-lesserangled-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-lesserangled-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-lesserangled-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-lesserangled-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-lesserangled-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-lesserangled-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-lesserangled-in-tabledata-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-lesserangled-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-lesserangled-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-lesserangled-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-lesserangled-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-lesserangled-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-lesserangled-in-slash-paren-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-lesserangled-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-lesserangled-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-lesserangled-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-lesserangled-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-lesserangled-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-lesserangled-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-lesserangled-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-lesserangled-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-lesserangled-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-lesserangled-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-lesserangled-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-lesserangled-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-lesserangled-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-lesserangled-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-lesserangled-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-lesserangled-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-lesserangled-in-slash-paren-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-lesserangled-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-lesserangled-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-lesserangled-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-lesserangled-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-lesserangled-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-lesserangled-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-lesserangled-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-lesserangled-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-lesserangled-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-lesserangled-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-lesserangled-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-lesserangled-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-lesserangled-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-lesserangled-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-lesserangled-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-lesserangled-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-lesserangled-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-lesserangled-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-lesserangled-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-lesserangled-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-lesserangled-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-lesserangled-in-xsl-template-atpt ()
  "Employ actions of  at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-lesserangled-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-lesserangled-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-lesserangled-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-lesserangled-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-lesserangled-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-lesserangled-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-lesserangled-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-lesserangled-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-lesserangled-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-lesserangled-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-lesserangled-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-lesserangled-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-lesserangled-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-lesserangled-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-lesserangled-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-lesserangled-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-lesserangled-in-xsl-template-atpt ()
  "Employ actions of END at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-lesserangled-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-lesserangled-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-lesserangled-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-lesserangled-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-lesserangled-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-lesserangled-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-lesserangled-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-lesserangled-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-lesserangled-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-lesserangled-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-lesserangled-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-lesserangled-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-lesserangled-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-lesserangled-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-lesserangled-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-lesserangled-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-lesserangled-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-lesserangled-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-lesserangled-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-lesserangled-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of LESSER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'lesserangled 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-greaterangled-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-greaterangled-in-blok-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-greaterangled-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-greaterangled-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-greaterangled-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-greaterangled-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-greaterangled-in-blok-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-greaterangled-in-blok-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-greaterangled-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-greaterangled-in-blok-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-greaterangled-in-blok-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-greaterangled-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-greaterangled-in-blok-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-greaterangled-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-greaterangled-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-greaterangled-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-greaterangled-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-greaterangled-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-greaterangled-in-blok-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-greaterangled-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-greaterangled-in-blok-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-greaterangled-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-greaterangled-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-greaterangled-in-blok-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-greaterangled-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-greaterangled-in-blok-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-greaterangled-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-greaterangled-in-blok-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-greaterangled-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-greaterangled-in-blok-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-greaterangled-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-greaterangled-in-blok-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-greaterangled-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-greaterangled-in-blok-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-greaterangled-in-blok-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-greaterangled-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-greaterangled-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-greaterangled-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-greaterangled-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-greaterangled-in-double-backslash-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-greaterangled-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-greaterangled-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-greaterangled-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-greaterangled-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-greaterangled-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-greaterangled-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-greaterangled-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-greaterangled-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-greaterangled-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-greaterangled-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-greaterangled-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-greaterangled-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-greaterangled-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-greaterangled-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-greaterangled-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-greaterangled-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-greaterangled-in-double-backslash-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-greaterangled-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-greaterangled-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-greaterangled-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-greaterangled-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-greaterangled-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-greaterangled-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-greaterangled-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-greaterangled-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-greaterangled-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-greaterangled-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-greaterangled-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-greaterangled-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-greaterangled-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-greaterangled-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-greaterangled-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-greaterangled-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-greaterangled-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-greaterangled-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-greaterangled-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-greaterangled-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-greaterangled-in-doubleslash-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-greaterangled-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-greaterangled-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-greaterangled-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-greaterangled-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-greaterangled-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-greaterangled-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-greaterangled-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-greaterangled-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-greaterangled-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-greaterangled-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-greaterangled-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-greaterangled-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-greaterangled-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-greaterangled-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-greaterangled-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-greaterangled-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-greaterangled-in-doubleslash-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-greaterangled-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-greaterangled-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-greaterangled-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-greaterangled-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-greaterangled-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-greaterangled-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-greaterangled-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-greaterangled-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-greaterangled-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-greaterangled-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-greaterangled-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-greaterangled-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-greaterangled-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-greaterangled-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-greaterangled-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-greaterangled-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-greaterangled-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-greaterangled-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-greaterangled-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-greaterangled-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-greaterangled-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-greaterangled-in-tabledata-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-greaterangled-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-greaterangled-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-greaterangled-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-greaterangled-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-greaterangled-in-tabledata-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-greaterangled-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-greaterangled-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-greaterangled-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-greaterangled-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-greaterangled-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-greaterangled-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-greaterangled-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-greaterangled-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-greaterangled-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-greaterangled-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-greaterangled-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-greaterangled-in-tabledata-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-greaterangled-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-greaterangled-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-greaterangled-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-greaterangled-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-greaterangled-in-tabledata-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-greaterangled-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-greaterangled-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-greaterangled-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-greaterangled-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-greaterangled-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-greaterangled-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-greaterangled-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-greaterangled-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-greaterangled-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-greaterangled-in-tabledata-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-greaterangled-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-greaterangled-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-greaterangled-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-greaterangled-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-greaterangled-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-greaterangled-in-slash-paren-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-greaterangled-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-greaterangled-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-greaterangled-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-greaterangled-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-greaterangled-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-greaterangled-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-greaterangled-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-greaterangled-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-greaterangled-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-greaterangled-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-greaterangled-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-greaterangled-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-greaterangled-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-greaterangled-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-greaterangled-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-greaterangled-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-greaterangled-in-slash-paren-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-greaterangled-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-greaterangled-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-greaterangled-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-greaterangled-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-greaterangled-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-greaterangled-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-greaterangled-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-greaterangled-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-greaterangled-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-greaterangled-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-greaterangled-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-greaterangled-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-greaterangled-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-greaterangled-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-greaterangled-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-greaterangled-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-greaterangled-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-greaterangled-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-greaterangled-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-greaterangled-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-greaterangled-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-greaterangled-in-xsl-template-atpt ()
  "Employ actions of  at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-greaterangled-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-greaterangled-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-greaterangled-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-greaterangled-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-greaterangled-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-greaterangled-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-greaterangled-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-greaterangled-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-greaterangled-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-greaterangled-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-greaterangled-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-greaterangled-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-greaterangled-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-greaterangled-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-greaterangled-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-greaterangled-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-greaterangled-in-xsl-template-atpt ()
  "Employ actions of END at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-greaterangled-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-greaterangled-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-greaterangled-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-greaterangled-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-greaterangled-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-greaterangled-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-greaterangled-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-greaterangled-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-greaterangled-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-greaterangled-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-greaterangled-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-greaterangled-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-greaterangled-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-greaterangled-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-greaterangled-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-greaterangled-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-greaterangled-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-greaterangled-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-greaterangled-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-greaterangled-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of GREATER-ANGLED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'greaterangled 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-left-right-singlequoted-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-left-right-singlequoted-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-left-right-singlequoted-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-left-right-singlequoted-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-left-right-singlequoted-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-left-right-singlequoted-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-left-right-singlequoted-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-left-right-singlequoted-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of  at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of END at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-left-right-singlequoted-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of LEFT-RIGHT-SINGLEQUOTED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'left-right-singlequoted 'xsl-template 'ar-th-whitespace (interactive-p)))

(defun ar-parentized-in-begin-end-quote-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th (interactive-p)))

(defun ar-greaterangle-parentized-in-begin-end-quote-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-parentized-in-begin-end-quote-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-parentized-in-begin-end-quote-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-backslash (interactive-p)))

(defun ar-backtick-parentized-in-begin-end-quote-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-backtick (interactive-p)))

(defun ar-beg-parentized-in-begin-end-quote-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-beg (interactive-p)))

(defun ar-blok-parentized-in-begin-end-quote-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-blok (interactive-p)))

(defun ar-bounds-parentized-in-begin-end-quote-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-bounds (interactive-p)))

(defun ar-brace-parentized-in-begin-end-quote-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-brace (interactive-p)))

(defun ar-bracket-parentized-in-begin-end-quote-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-bracket (interactive-p)))

(defun ar-commatize-parentized-in-begin-end-quote-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-commatize (interactive-p)))

(defun ar-comment-parentized-in-begin-end-quote-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-comment (interactive-p)))

(defun ar-dollar-parentized-in-begin-end-quote-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-parentized-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-parentized-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-parentized-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-parentized-in-begin-end-quote-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-parentized-in-begin-end-quote-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-end (interactive-p)))

(defun ar-escape-parentized-in-begin-end-quote-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-escape (interactive-p)))

(defun ar-hide-parentized-in-begin-end-quote-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-hide (interactive-p)))

(defun ar-hide-show-parentized-in-begin-end-quote-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-parentized-in-begin-end-quote-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-hyphen (interactive-p)))

(defun ar-kill-parentized-in-begin-end-quote-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-parentized-in-begin-end-quote-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-parentized-in-begin-end-quote-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-length (interactive-p)))

(defun ar-parentize-parentized-in-begin-end-quote-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-parentize (interactive-p)))

(defun ar-quote-parentized-in-begin-end-quote-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-quote (interactive-p)))

(defun ar-separate-parentized-in-begin-end-quote-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-separate (interactive-p)))

(defun ar-show-parentized-in-begin-end-quote-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-show (interactive-p)))

(defun ar-singlequote-parentized-in-begin-end-quote-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-singlequote (interactive-p)))

(defun ar-slash-parentized-in-begin-end-quote-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-slash (interactive-p)))

(defun ar-slashparen-parentized-in-begin-end-quote-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-slashparen (interactive-p)))

(defun ar-sort-parentized-in-begin-end-quote-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-sort (interactive-p)))

(defun ar-trim-parentized-in-begin-end-quote-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-trim (interactive-p)))

(defun ar-trim-left-parentized-in-begin-end-quote-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-parentized-in-begin-end-quote-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-parentized-in-begin-end-quote-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-parentized-in-begin-end-quote-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing BEGIN-END-QUOTE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'begin-end-quote 'ar-th-whitespace (interactive-p)))

(defun ar-parentized-in-blok-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th (interactive-p)))

(defun ar-greaterangle-parentized-in-blok-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-parentized-in-blok-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-parentized-in-blok-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-backslash (interactive-p)))

(defun ar-backtick-parentized-in-blok-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-backtick (interactive-p)))

(defun ar-beg-parentized-in-blok-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-beg (interactive-p)))

(defun ar-blok-parentized-in-blok-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-blok (interactive-p)))

(defun ar-bounds-parentized-in-blok-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-bounds (interactive-p)))

(defun ar-brace-parentized-in-blok-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-brace (interactive-p)))

(defun ar-bracket-parentized-in-blok-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-bracket (interactive-p)))

(defun ar-commatize-parentized-in-blok-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-commatize (interactive-p)))

(defun ar-comment-parentized-in-blok-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-comment (interactive-p)))

(defun ar-dollar-parentized-in-blok-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-parentized-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-parentized-in-blok-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-parentized-in-blok-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-parentized-in-blok-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-parentized-in-blok-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-end (interactive-p)))

(defun ar-escape-parentized-in-blok-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-escape (interactive-p)))

(defun ar-hide-parentized-in-blok-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-hide (interactive-p)))

(defun ar-hide-show-parentized-in-blok-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-parentized-in-blok-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-hyphen (interactive-p)))

(defun ar-kill-parentized-in-blok-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-parentized-in-blok-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-parentized-in-blok-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-length (interactive-p)))

(defun ar-parentize-parentized-in-blok-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-parentize (interactive-p)))

(defun ar-quote-parentized-in-blok-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-quote (interactive-p)))

(defun ar-separate-parentized-in-blok-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-separate (interactive-p)))

(defun ar-show-parentized-in-blok-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-show (interactive-p)))

(defun ar-singlequote-parentized-in-blok-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-singlequote (interactive-p)))

(defun ar-slash-parentized-in-blok-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-slash (interactive-p)))

(defun ar-slashparen-parentized-in-blok-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-slashparen (interactive-p)))

(defun ar-sort-parentized-in-blok-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-sort (interactive-p)))

(defun ar-trim-parentized-in-blok-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-trim (interactive-p)))

(defun ar-trim-left-parentized-in-blok-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-parentized-in-blok-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-parentized-in-blok-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-parentized-in-blok-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing BLOK. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'blok 'ar-th-whitespace (interactive-p)))

(defun ar-parentized-in-double-backslash-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th (interactive-p)))

(defun ar-greaterangle-parentized-in-double-backslash-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-parentized-in-double-backslash-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-parentized-in-double-backslash-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-parentized-in-double-backslash-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-parentized-in-double-backslash-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-beg (interactive-p)))

(defun ar-blok-parentized-in-double-backslash-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-parentized-in-double-backslash-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-parentized-in-double-backslash-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-parentized-in-double-backslash-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-parentized-in-double-backslash-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-parentized-in-double-backslash-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-parentized-in-double-backslash-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-parentized-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-parentized-in-double-backslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-parentized-in-double-backslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-parentized-in-double-backslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-parentized-in-double-backslash-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-end (interactive-p)))

(defun ar-escape-parentized-in-double-backslash-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-escape (interactive-p)))

(defun ar-hide-parentized-in-double-backslash-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-parentized-in-double-backslash-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-parentized-in-double-backslash-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-parentized-in-double-backslash-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-parentized-in-double-backslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-parentized-in-double-backslash-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-length (interactive-p)))

(defun ar-parentize-parentized-in-double-backslash-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-parentized-in-double-backslash-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-quote (interactive-p)))

(defun ar-separate-parentized-in-double-backslash-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-separate (interactive-p)))

(defun ar-show-parentized-in-double-backslash-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-parentized-in-double-backslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-parentized-in-double-backslash-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-parentized-in-double-backslash-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-parentized-in-double-backslash-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-sort (interactive-p)))

(defun ar-trim-parentized-in-double-backslash-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-parentized-in-double-backslash-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-parentized-in-double-backslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-parentized-in-double-backslash-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-parentized-in-double-backslash-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash 'ar-th-whitespace (interactive-p)))

(defun ar-parentized-in-doubleslash-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th (interactive-p)))

(defun ar-greaterangle-parentized-in-doubleslash-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-parentized-in-doubleslash-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-parentized-in-doubleslash-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-backslash (interactive-p)))

(defun ar-backtick-parentized-in-doubleslash-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-backtick (interactive-p)))

(defun ar-beg-parentized-in-doubleslash-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-beg (interactive-p)))

(defun ar-blok-parentized-in-doubleslash-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-blok (interactive-p)))

(defun ar-bounds-parentized-in-doubleslash-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-bounds (interactive-p)))

(defun ar-brace-parentized-in-doubleslash-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-brace (interactive-p)))

(defun ar-bracket-parentized-in-doubleslash-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-bracket (interactive-p)))

(defun ar-commatize-parentized-in-doubleslash-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-commatize (interactive-p)))

(defun ar-comment-parentized-in-doubleslash-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-comment (interactive-p)))

(defun ar-dollar-parentized-in-doubleslash-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-parentized-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-parentized-in-doubleslash-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-parentized-in-doubleslash-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-parentized-in-doubleslash-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-parentized-in-doubleslash-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-end (interactive-p)))

(defun ar-escape-parentized-in-doubleslash-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-escape (interactive-p)))

(defun ar-hide-parentized-in-doubleslash-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-hide (interactive-p)))

(defun ar-hide-show-parentized-in-doubleslash-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-parentized-in-doubleslash-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-hyphen (interactive-p)))

(defun ar-kill-parentized-in-doubleslash-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-parentized-in-doubleslash-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-parentized-in-doubleslash-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-length (interactive-p)))

(defun ar-parentize-parentized-in-doubleslash-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-parentize (interactive-p)))

(defun ar-quote-parentized-in-doubleslash-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-quote (interactive-p)))

(defun ar-separate-parentized-in-doubleslash-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-separate (interactive-p)))

(defun ar-show-parentized-in-doubleslash-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-show (interactive-p)))

(defun ar-singlequote-parentized-in-doubleslash-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-singlequote (interactive-p)))

(defun ar-slash-parentized-in-doubleslash-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-slash (interactive-p)))

(defun ar-slashparen-parentized-in-doubleslash-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-slashparen (interactive-p)))

(defun ar-sort-parentized-in-doubleslash-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-sort (interactive-p)))

(defun ar-trim-parentized-in-doubleslash-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-trim (interactive-p)))

(defun ar-trim-left-parentized-in-doubleslash-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-parentized-in-doubleslash-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-parentized-in-doubleslash-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-parentized-in-doubleslash-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing DOUBLESLASH. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'doubleslash 'ar-th-whitespace (interactive-p)))

(defun ar-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-parentized-in-double-backslash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing DOUBLE-BACKSLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'double-backslash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-parentized-in-tabledata-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th (interactive-p)))

(defun ar-greaterangle-parentized-in-tabledata-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-parentized-in-tabledata-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-parentized-in-tabledata-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-backslash (interactive-p)))

(defun ar-backtick-parentized-in-tabledata-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-backtick (interactive-p)))

(defun ar-beg-parentized-in-tabledata-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-beg (interactive-p)))

(defun ar-blok-parentized-in-tabledata-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-blok (interactive-p)))

(defun ar-bounds-parentized-in-tabledata-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-bounds (interactive-p)))

(defun ar-brace-parentized-in-tabledata-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-brace (interactive-p)))

(defun ar-bracket-parentized-in-tabledata-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-bracket (interactive-p)))

(defun ar-commatize-parentized-in-tabledata-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-commatize (interactive-p)))

(defun ar-comment-parentized-in-tabledata-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-comment (interactive-p)))

(defun ar-dollar-parentized-in-tabledata-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-parentized-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-parentized-in-tabledata-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-parentized-in-tabledata-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-parentized-in-tabledata-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-parentized-in-tabledata-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-end (interactive-p)))

(defun ar-escape-parentized-in-tabledata-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-escape (interactive-p)))

(defun ar-hide-parentized-in-tabledata-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-hide (interactive-p)))

(defun ar-hide-show-parentized-in-tabledata-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-parentized-in-tabledata-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-hyphen (interactive-p)))

(defun ar-kill-parentized-in-tabledata-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-parentized-in-tabledata-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-parentized-in-tabledata-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-length (interactive-p)))

(defun ar-parentize-parentized-in-tabledata-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-parentize (interactive-p)))

(defun ar-quote-parentized-in-tabledata-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-quote (interactive-p)))

(defun ar-separate-parentized-in-tabledata-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-separate (interactive-p)))

(defun ar-show-parentized-in-tabledata-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-show (interactive-p)))

(defun ar-singlequote-parentized-in-tabledata-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-singlequote (interactive-p)))

(defun ar-slash-parentized-in-tabledata-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-slash (interactive-p)))

(defun ar-slashparen-parentized-in-tabledata-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-slashparen (interactive-p)))

(defun ar-sort-parentized-in-tabledata-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-sort (interactive-p)))

(defun ar-trim-parentized-in-tabledata-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-trim (interactive-p)))

(defun ar-trim-left-parentized-in-tabledata-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-parentized-in-tabledata-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-parentized-in-tabledata-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-parentized-in-tabledata-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing TABLEDATA. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'tabledata 'ar-th-whitespace (interactive-p)))

(defun ar-parentized-in-slash-paren-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th (interactive-p)))

(defun ar-greaterangle-parentized-in-slash-paren-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-parentized-in-slash-paren-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-parentized-in-slash-paren-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-backslash (interactive-p)))

(defun ar-backtick-parentized-in-slash-paren-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-backtick (interactive-p)))

(defun ar-beg-parentized-in-slash-paren-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-beg (interactive-p)))

(defun ar-blok-parentized-in-slash-paren-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-blok (interactive-p)))

(defun ar-bounds-parentized-in-slash-paren-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-bounds (interactive-p)))

(defun ar-brace-parentized-in-slash-paren-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-brace (interactive-p)))

(defun ar-bracket-parentized-in-slash-paren-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-bracket (interactive-p)))

(defun ar-commatize-parentized-in-slash-paren-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-commatize (interactive-p)))

(defun ar-comment-parentized-in-slash-paren-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-comment (interactive-p)))

(defun ar-dollar-parentized-in-slash-paren-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-parentized-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-parentized-in-slash-paren-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-parentized-in-slash-paren-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-parentized-in-slash-paren-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-parentized-in-slash-paren-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-end (interactive-p)))

(defun ar-escape-parentized-in-slash-paren-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-escape (interactive-p)))

(defun ar-hide-parentized-in-slash-paren-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-hide (interactive-p)))

(defun ar-hide-show-parentized-in-slash-paren-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-parentized-in-slash-paren-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-hyphen (interactive-p)))

(defun ar-kill-parentized-in-slash-paren-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-parentized-in-slash-paren-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-parentized-in-slash-paren-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-length (interactive-p)))

(defun ar-parentize-parentized-in-slash-paren-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-parentize (interactive-p)))

(defun ar-quote-parentized-in-slash-paren-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-quote (interactive-p)))

(defun ar-separate-parentized-in-slash-paren-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-separate (interactive-p)))

(defun ar-show-parentized-in-slash-paren-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-show (interactive-p)))

(defun ar-singlequote-parentized-in-slash-paren-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-singlequote (interactive-p)))

(defun ar-slash-parentized-in-slash-paren-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-slash (interactive-p)))

(defun ar-slashparen-parentized-in-slash-paren-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-slashparen (interactive-p)))

(defun ar-sort-parentized-in-slash-paren-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-sort (interactive-p)))

(defun ar-trim-parentized-in-slash-paren-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-trim (interactive-p)))

(defun ar-trim-left-parentized-in-slash-paren-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-parentized-in-slash-paren-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-parentized-in-slash-paren-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-parentized-in-slash-paren-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing SLASH-PAREN. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'slash-paren 'ar-th-whitespace (interactive-p)))

(defun ar-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th (interactive-p)))

(defun ar-greaterangle-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-backslash (interactive-p)))

(defun ar-backtick-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-backtick (interactive-p)))

(defun ar-beg-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-beg (interactive-p)))

(defun ar-blok-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-blok (interactive-p)))

(defun ar-bounds-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-bounds (interactive-p)))

(defun ar-brace-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-brace (interactive-p)))

(defun ar-bracket-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-bracket (interactive-p)))

(defun ar-commatize-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-commatize (interactive-p)))

(defun ar-comment-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-comment (interactive-p)))

(defun ar-dollar-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-end (interactive-p)))

(defun ar-escape-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-escape (interactive-p)))

(defun ar-hide-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-hide (interactive-p)))

(defun ar-hide-show-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-hyphen (interactive-p)))

(defun ar-kill-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-length (interactive-p)))

(defun ar-parentize-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-parentize (interactive-p)))

(defun ar-quote-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-quote (interactive-p)))

(defun ar-separate-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-separate (interactive-p)))

(defun ar-show-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-show (interactive-p)))

(defun ar-singlequote-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-singlequote (interactive-p)))

(defun ar-slash-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-slash (interactive-p)))

(defun ar-slashparen-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-slashparen (interactive-p)))

(defun ar-sort-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-sort (interactive-p)))

(defun ar-trim-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-trim (interactive-p)))

(defun ar-trim-left-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-parentized-in-xsl-stylesheet-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing XSL-STYLESHEET. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-stylesheet 'ar-th-whitespace (interactive-p)))

(defun ar-parentized-in-xsl-template-atpt ()
  "Employ actions of  at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th (interactive-p)))

(defun ar-greaterangle-parentized-in-xsl-template-atpt ()
  "Employ actions of GREATERANGLE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-greaterangle (interactive-p)))

(defun ar-lesserangle-parentized-in-xsl-template-atpt ()
  "Employ actions of LESSERANGLE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-lesserangle (interactive-p)))

(defun ar-backslash-parentized-in-xsl-template-atpt ()
  "Employ actions of BACKSLASH at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-backslash (interactive-p)))

(defun ar-backtick-parentized-in-xsl-template-atpt ()
  "Employ actions of BACKTICK at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-backtick (interactive-p)))

(defun ar-beg-parentized-in-xsl-template-atpt ()
  "Employ actions of BEG at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-beg (interactive-p)))

(defun ar-blok-parentized-in-xsl-template-atpt ()
  "Employ actions of BLOK at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-blok (interactive-p)))

(defun ar-bounds-parentized-in-xsl-template-atpt ()
  "Employ actions of BOUNDS at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-bounds (interactive-p)))

(defun ar-brace-parentized-in-xsl-template-atpt ()
  "Employ actions of BRACE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-brace (interactive-p)))

(defun ar-bracket-parentized-in-xsl-template-atpt ()
  "Employ actions of BRACKET at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-bracket (interactive-p)))

(defun ar-commatize-parentized-in-xsl-template-atpt ()
  "Employ actions of COMMATIZE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-commatize (interactive-p)))

(defun ar-comment-parentized-in-xsl-template-atpt ()
  "Employ actions of COMMENT at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-comment (interactive-p)))

(defun ar-dollar-parentized-in-xsl-template-atpt ()
  "Employ actions of DOLLAR at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-dollar (interactive-p)))

(defun ar-double-backslash-parentized-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASH at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-double-backslash (interactive-p)))

(defun ar-doublequote-parentized-in-xsl-template-atpt ()
  "Employ actions of DOUBLEQUOTE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-doublequote (interactive-p)))

(defun ar-doubleslash-parentized-in-xsl-template-atpt ()
  "Employ actions of DOUBLESLASH at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-doubleslash (interactive-p)))

(defun ar-double-backslash-paren-parentized-in-xsl-template-atpt ()
  "Employ actions of DOUBLEBACKSLASHPAREN at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-double-backslash-paren (interactive-p)))

(defun ar-end-parentized-in-xsl-template-atpt ()
  "Employ actions of END at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-end (interactive-p)))

(defun ar-escape-parentized-in-xsl-template-atpt ()
  "Employ actions of ESCAPE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-escape (interactive-p)))

(defun ar-hide-parentized-in-xsl-template-atpt ()
  "Employ actions of HIDE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-hide (interactive-p)))

(defun ar-hide-show-parentized-in-xsl-template-atpt ()
  "Employ actions of HIDESHOW at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-hide-show (interactive-p)))

(defun ar-hyphen-parentized-in-xsl-template-atpt ()
  "Employ actions of HYPHEN at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-hyphen (interactive-p)))

(defun ar-kill-parentized-in-xsl-template-atpt ()
  "Employ actions of KILL at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-kill (interactive-p)))

(defun ar-left-right-singlequote-parentized-in-xsl-template-atpt ()
  "Employ actions of LEFTRIGHTSINGLEQUOTE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-left-right-singlequote (interactive-p)))

(defun ar-length-parentized-in-xsl-template-atpt ()
  "Employ actions of LENGTH at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-length (interactive-p)))

(defun ar-parentize-parentized-in-xsl-template-atpt ()
  "Employ actions of PARENTIZE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-parentize (interactive-p)))

(defun ar-quote-parentized-in-xsl-template-atpt ()
  "Employ actions of QUOTE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-quote (interactive-p)))

(defun ar-separate-parentized-in-xsl-template-atpt ()
  "Employ actions of SEPARATE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-separate (interactive-p)))

(defun ar-show-parentized-in-xsl-template-atpt ()
  "Employ actions of SHOW at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-show (interactive-p)))

(defun ar-singlequote-parentized-in-xsl-template-atpt ()
  "Employ actions of SINGLEQUOTE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-singlequote (interactive-p)))

(defun ar-slash-parentized-in-xsl-template-atpt ()
  "Employ actions of SLASH at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-slash (interactive-p)))

(defun ar-slashparen-parentized-in-xsl-template-atpt ()
  "Employ actions of SLASHPAREN at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-slashparen (interactive-p)))

(defun ar-sort-parentized-in-xsl-template-atpt ()
  "Employ actions of SORT at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-sort (interactive-p)))

(defun ar-trim-parentized-in-xsl-template-atpt ()
  "Employ actions of TRIM at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-trim (interactive-p)))

(defun ar-trim-left-parentized-in-xsl-template-atpt ()
  "Employ actions of TRIMLEFT at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-trim-left (interactive-p)))

(defun ar-trim-right-parentized-in-xsl-template-atpt ()
  "Employ actions of TRIMRIGHT at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-trim-right (interactive-p)))

(defun ar-underscore-parentized-in-xsl-template-atpt ()
  "Employ actions of UNDERSCORE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-underscore (interactive-p)))

(defun ar-whitespace-parentized-in-xsl-template-atpt ()
  "Employ actions of WHITESPACE at things class of PARENTIZED residing withing XSL-TEMPLATE. "
  (interactive "*")
  (ar-thing-in-thing 'parentized 'xsl-template 'ar-th-whitespace (interactive-p)))

(provide 'ar-thingatpt-delimited-list-in-data-forms-aktiv)
;;;thing-delimited-list-in-data-forms-aktiv.el ends here

